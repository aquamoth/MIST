using Mono.Cecil;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Mathtone.MIST;
using Mono.Cecil.Cil;
using System.Reflection;

namespace Mathtone.MIST.Processors {

	public class PropertyProcessor : IDefinitionProcessor<PropertyDefinition> {

		NotificationMode defaultMode;
		NotificationStyle defaultStyle;
		MethodReference notifyTarget;
		
		//static MethodInfo equalsMethod = typeof(object).GetMethods().FirstOrDefault(a => a.Name == "Equals" && !a.IsStatic);
		public bool ContainsChanges { get; protected set; }

		public PropertyProcessor(MethodReference target, NotificationMode mode, NotificationStyle style) {
			this.notifyTarget = target;
			this.defaultMode = mode;
			this.defaultStyle = style;
		}

		public void Process(PropertyDefinition property) {
			var strategy = new PropertyStrategy(property, defaultMode, defaultStyle, notifyTarget);
			//var strategy = GetStrategy(property, defaultMode, notifyTarget);
			if (!strategy.IsIgnored) {
				ImplementStrategy(strategy);
			}
		}

		void ImplementStrategy(ImplementationStrategy strategy) {
			//Just in case...
			if (strategy.IsIgnored) {
				return;
			}
			if (strategy.Property.SetMethod.IsAbstract) {
				//This is an abstract property, we don't do these.
				throw new InvalidNotifierException();
			}

			switch (strategy.ImplementationStyle) {
				case ImplementationStyle.Inline:
					ImplementInline(strategy);
					break;
				case ImplementationStyle.Wrapped:
					ImplementWrapped(strategy);
					break;
				default: throw new NotImplementedException();
			}
			ContainsChanges = true;
		}

		static void ImplementInline(ImplementationStrategy strategy) {

			var method = strategy.Property.SetMethod;
			var msil = method.Body.GetILProcessor();
			var begin = method.Body.Instructions.First();
			var end = method.Body.Instructions.Last();

			if (strategy.NotificationStyle == NotificationStyle.OnSet) {
				InsertBefore(msil, msil.Create(OpCodes.Nop), begin);
				InsertBefore(msil, CallNotifyTargetInstructions(msil, strategy), end);
			}
			else {
				throw new BuildTaskErrorException("Inline implementation does not support OnChange notification");
			}
		}

		static IEnumerable<Instruction> CallNotifyTargetInstructions(ILProcessor ilProcessor, ImplementationStrategy strategy) {
			foreach (var name in strategy.NotifyValues) {

				yield return ilProcessor.Create(OpCodes.Ldarg_0);


				if (strategy.NotifyTarget.Parameters.Count > 0) {
					yield return ilProcessor.Create(OpCodes.Ldstr, name);
				}
				if (strategy.NotifyTarget.Parameters.Count > 1) {
					if (strategy.Property.PropertyType.IsValueType) {
						yield return ilProcessor.Create(OpCodes.Box,strategy.NotifyTarget.Parameters[1].ParameterType);
					}
					yield return ilProcessor.Create(OpCodes.Ldarg_1);
				}
					var opCode = strategy.NotifyTargetDefinition.IsVirtual ?
						OpCodes.Callvirt : OpCodes.Call;
					yield return ilProcessor.Create(opCode, strategy.NotifyTarget);
				yield return ilProcessor.Create(OpCodes.Nop);
			}
		}

		static void ImplementWrapped(ImplementationStrategy strategy) {
            //Rename the original set-method and create a new one to be used as a wrapper
			var originalSetMethod = strategy.Property.SetMethod;
			var newSetMethod = new MethodDefinition(originalSetMethod.Name, originalSetMethod.Attributes, originalSetMethod.ReturnType);
            newSetMethod.DeclaringType = originalSetMethod.DeclaringType;
            newSetMethod.Parameters.Add(new ParameterDefinition(originalSetMethod.Parameters[0].ParameterType));
            originalSetMethod.Name = $"{originalSetMethod.Name}`mist"; // "misted`" + setMethod.Name;
            strategy.Property.SetMethod = newSetMethod;
            newSetMethod.DeclaringType.Methods.Add(newSetMethod);


            Instruction[] instructions;
            var msil = newSetMethod.Body.GetILProcessor();
            switch (strategy.NotificationStyle)
            {
                case NotificationStyle.OnSet:
                    instructions = ImplementWrapped_OnSet(msil, strategy, originalSetMethod).ToArray();
                    break;
                case NotificationStyle.OnChange:
                    instructions = ImplementWrapped_OnChange(msil, strategy, originalSetMethod, newSetMethod).ToArray();
                    break;
                default:
                    throw new NotSupportedException();
            }

            foreach (var instruction in instructions) {
				newSetMethod.Body.Instructions.Add(instruction);
			}
		}

        private static IEnumerable<Instruction> ImplementWrapped_OnSet(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod)
        {
            return new[] {
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Ldarg_1),
                msil.Create(OpCodes.Call, originalSetMethod),
            }
            .Concat(CallNotifyTargetInstructions(msil, strategy))
            .Concat(new[] { msil.Create(OpCodes.Ret) });
        }

        private static IEnumerable<Instruction> ImplementWrapped_OnChange(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodDefinition newSetMethod)
        {
            var boolType = strategy.Property.Module.ImportReference(typeof(bool));
				

            var propertyType = strategy.Property.PropertyType.Resolve();

            //find equals method
            var equalsMethod = SeekMethod(
                propertyType,
                a =>
                    a.Name == "Equals" &&
                    a.Parameters.Count == 1
            );
            var equality = strategy.Property.Module.ImportReference(equalsMethod);

            var equalityReference = equalsMethod.Resolve();

            var v1 = new VariableDefinition(strategy.Property.PropertyType);
            var v2 = new VariableDefinition(boolType);
            var v3 = new VariableDefinition(boolType);

            newSetMethod.Body.Variables.Add(v1);
            newSetMethod.Body.Variables.Add(v2);
            newSetMethod.Body.Variables.Add(v3);

            var instructions = new List<Instruction>();
            var rtn = msil.Create(OpCodes.Ret);
            if (propertyType.IsPrimitive)
            {
                instructions.AddRange(ImplementWrapped_InChange_Primitive(msil, strategy, originalSetMethod, equality, rtn));
            }
            else
            {
                instructions.AddRange(ImplementWrapped_OnChange_ComplexType(msil, strategy, originalSetMethod, equality, equalityReference, rtn));
            };

            instructions.AddRange(CallNotifyTargetInstructions(msil, strategy));
            instructions.Add(msil.Create(OpCodes.Nop));
            instructions.Add(rtn);
            return instructions;
        }

        private static Instruction[] ImplementWrapped_InChange_Primitive(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodReference equality, Instruction rtn)
        {
            return new[] {
                //msil.Create(OpCodes.Nop),
                //msil.Create(OpCodes.Ldarg_0),
                //msil.Create(OpCodes.Call,strategy.Property.GetMethod),
                //msil.Create(OpCodes.Stloc_0),

                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Ldarg_1),
                msil.Create(OpCodes.Call, originalSetMethod),

                //msil.Create(OpCodes.Nop),
                //msil.Create(OpCodes.Ldloc_0),
                //msil.Create(OpCodes.Ldarg_1),
                //msil.Create(OpCodes.Call, equality),
                ////msil.Create(OpCodes.Ceq),
                //msil.Create(OpCodes.Ldc_I4_0),
                //msil.Create(OpCodes.Ceq),
                //msil.Create(OpCodes.Stloc_1),
                //msil.Create(OpCodes.Ldloc_1),
                //msil.Create(OpCodes.Brfalse_S,rtn),
                //msil.Create(OpCodes.Nop)
            };
        }

        private static Instruction[] ImplementWrapped_OnChange_ComplexType(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodReference equality, MethodDefinition equalityReference, Instruction rtn)
        {
            return new[] {
                //msil.Create(OpCodes.Nop),
                //msil.Create(OpCodes.Ldarg_0),
                //msil.Create(OpCodes.Call,strategy.Property.GetMethod),
                //msil.Create(OpCodes.Stloc_0),

                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Ldarg_1),
                msil.Create(OpCodes.Call, originalSetMethod),

                //msil.Create(OpCodes.Nop),
                //msil.Create(OpCodes.Ldloc_0),
                //msil.Create(OpCodes.Ldarg_1),
                //msil.Create(equalityReference.IsVirtual? OpCodes.Callvirt:OpCodes.Call,equality),
                //msil.Create(OpCodes.Stloc_1),
                //msil.Create(OpCodes.Ldloc_1),
                //msil.Create(OpCodes.Brfalse_S,rtn),
                //msil.Create(OpCodes.Nop)
            };
        }

        static IEnumerable<MethodDefinition> GetMethods(TypeDefinition type, Func<MethodDefinition, bool> evaluator) =>
			type.Methods.Where(a => evaluator(a));

		static MethodDefinition SeekMethod(TypeDefinition type, Func<MethodDefinition, bool> evaluator) {

			var rtn = GetMethods(type, evaluator).FirstOrDefault();
			if (rtn == null && type.BaseType != null) {
				rtn = SeekMethod(type.BaseType.Resolve(),evaluator);
			}
			return rtn;
			//MethodDefinition rtn;

		}

		static void InsertAfter(ILProcessor ilProcessor, IEnumerable<Instruction> instructions, Instruction insertionPoint) {
			var currentInstruction = insertionPoint;
			foreach (var instruction in instructions) {
				ilProcessor.InsertAfter(currentInstruction, instruction);
				currentInstruction = instruction;
			}
		}

		static void InsertAfter(ILProcessor ilProcessor, Instruction instruction, Instruction insertionPoint) =>
			InsertAfter(ilProcessor, new[] { instruction }, insertionPoint);

		static void InsertBefore(ILProcessor ilProcessor, Instruction instruction, Instruction insertionPoint) =>
			InsertBefore(ilProcessor, new[] { instruction }, insertionPoint);

		static void InsertBefore(ILProcessor ilProcessor, IEnumerable<Instruction> instructions, Instruction insertionPoint) {
			var currentInstruction = null as Instruction;
			foreach (var instruction in instructions) {
				if (currentInstruction == null) {
					ilProcessor.InsertBefore(insertionPoint, instruction);
				}
				else {
					ilProcessor.InsertAfter(currentInstruction, instruction);
				}
				currentInstruction = instruction;
			}
		}
	}
}