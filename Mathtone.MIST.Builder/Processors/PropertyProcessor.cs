using Mono.Cecil;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Mathtone.MIST;
using Mono.Cecil.Cil;
using System.Reflection;
using Mathtone.MIST.Helpers;

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
				

            var propertyType = strategy.Property.PropertyType.Resolve();

            //find equals method
            var equalsMethod = SeekMethod(propertyType, a => a.Name == "Equals" && a.Parameters.Count == 1);
            var equality = strategy.Property.Module.ImportReference(equalsMethod);
            var equalityReference = equalsMethod.Resolve();

            IEnumerable<Instruction> instructions;
            var rtn = msil.Create(OpCodes.Ret);
            if (propertyType.IsPrimitive)
            {
                instructions = ImplementWrapped_InChange_Primitive(msil, strategy, originalSetMethod, equality, rtn);
            }
            else if (!propertyType.IsValueType)
            {
                instructions = ImplementWrapped_OnChange_ReferenceType(msil, strategy, originalSetMethod, equality, equalityReference, rtn);
            }
            else if (SeekMethod(propertyType, a => a.Name == "get_HasValue" && a.Parameters.Count == 0) != null)
            {
                instructions = ImplementWrapped_OnChange_NullableValueType(msil, strategy, originalSetMethod, equality, equalityReference, rtn);
            }
            else
            {
#warning Placeholder code!
                instructions = new[] {
                    msil.Create(OpCodes.Ldarg_0),
                    msil.Create(OpCodes.Ldarg_1),
                    msil.Create(OpCodes.Call, originalSetMethod),
                };
            };

            return instructions
                .Concat(CallNotifyTargetInstructions(msil, strategy))
                .Concat(new[] { msil.Create(OpCodes.Nop), rtn });
        }

        private static IEnumerable<Instruction> ImplementWrapped_InChange_Primitive(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodReference equality, Instruction rtn)
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

        private static IEnumerable<Instruction> ImplementWrapped_OnChange_NullableValueType(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodReference equality, MethodDefinition equalityReference, Instruction rtn)
        {

            //Register needed variables
            var V_0 = new VariableDefinition(strategy.Property.PropertyType);
            msil.Body.Variables.Add(V_0);
            var V_1 = new VariableDefinition(strategy.Property.PropertyType);
            msil.Body.Variables.Add(V_1);

            var propertyTypeDefinition = strategy.Property.PropertyType.Resolve();
            var propertyTypeModule = propertyTypeDefinition.Module;

            var intType = strategy.Property.Module.ImportReference(typeof(int));
            var intReference = intType.Resolve();// propertyTypeModule.TypeSystem.Int32;

            var get_HasValueDefinition = propertyTypeDefinition.Methods.Single(m => m.Name == "get_HasValue");
            var get_HasValueReference = strategy.Property.Module
                .ImportReference(get_HasValueDefinition)
                .MakeGeneric(intReference);

            var GetValueOrDefaultDefinition = propertyTypeDefinition.Methods.Single(m => m.Name == "GetValueOrDefault" && m.Parameters.Count == 0);
            var GetValueOrDefaultReference = strategy.Property.Module
                .ImportReference(GetValueOrDefaultDefinition)
                .MakeGeneric(intReference);


            ////var GetValueOrDefaultBaseMethod = SeekMethod(propertyType, a => a.Name == "GetValueOrDefault" && a.Parameters.Count == 0);
            ////var GetValueOrDefaultMethod = strategy.Property.Module.ImportReference(GetValueOrDefaultBaseMethod);
            ////var getValueOrDefaultReference = GetValueOrDefaultBaseMethod.Resolve();
            //var GetValueOrDefaultDefinition = propertyTypeDefinition.Methods.Single(m => m.Name == "GetValueOrDefault" && m.Parameters.Count == 0);
            //var GetValueOrDefaultReference = GetValueOrDefaultDefinition.Module.ImportReference(GetValueOrDefaultDefinition);
            //if (propertyTypeDefinition.BaseType.IsGenericInstance)
            //{
            //    var baseTypeInstance = (GenericInstanceType)propertyTypeDefinition.BaseType;
            //    GetValueOrDefaultReference = GetValueOrDefaultReference.MakeGeneric(baseTypeInstance.GenericArguments.ToArray());
            //}


#warning Exact value-type should be derived from PropertyType, only non-nullable
            var V_2 = new VariableDefinition(intType);
            msil.Body.Variables.Add(V_2);

            //Code working for nullable ValueTypes
            var IL_0027 = new[]
            {
                msil.Create(OpCodes.Ldloca_S, V_0),            
                msil.Create(OpCodes.Call, GetValueOrDefaultReference),  
                msil.Create(OpCodes.Stloc_2),
                msil.Create(OpCodes.Ldloca_S, V_2),                
                msil.Create(OpCodes.Ldloc_1),
                msil.Create(OpCodes.Box, strategy.Property.PropertyType),
                msil.Create(equalityReference.IsVirtual ? OpCodes.Callvirt : OpCodes.Call, equality),
                msil.Create(OpCodes.Ldc_I4_0),
                msil.Create(OpCodes.Ceq),
            };
            var IL_003f = new[]
            {
                msil.Create(OpCodes.Brfalse_S, rtn),
            };

            return new[] {
                msil.Create(OpCodes.Nop),

                //V_0 = Property-Get
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Call, strategy.Property.GetMethod),
                msil.Create(OpCodes.Stloc_0),

                //Original Property-Set = value
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Ldarg_1),
                msil.Create(OpCodes.Call, originalSetMethod),

                //V_1 = Property-Get
                msil.Create(OpCodes.Nop),
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Call, strategy.Property.GetMethod),
                msil.Create(OpCodes.Stloc_1),

                //if (!V_0?.Equals(V_1) ?? (V_1 != null))
                msil.Create(OpCodes.Ldloca_S, V_0),             
                msil.Create(OpCodes.Call, get_HasValueReference),            
                msil.Create(OpCodes.Brtrue_S, IL_0027.First()), 
                msil.Create(OpCodes.Ldloca_S, V_1),             
                msil.Create(OpCodes.Call, get_HasValueReference),            
                msil.Create(OpCodes.Brtrue_S, IL_003f.First()),      
            }
            .Concat(IL_0027)
            .Concat(IL_003f);
        }

        private static IEnumerable<Instruction> ImplementWrapped_OnChange_ReferenceType(ILProcessor msil, ImplementationStrategy strategy, MethodDefinition originalSetMethod, MethodReference equality, MethodDefinition equalityReference, Instruction rtn)
        {
            //Register needed variables
            var boolType = strategy.Property.Module.ImportReference(typeof(bool));
            msil.Body.Variables.Add(new VariableDefinition(strategy.Property.PropertyType));
            msil.Body.Variables.Add(new VariableDefinition(strategy.Property.PropertyType));
            msil.Body.Variables.Add(new VariableDefinition(boolType));

            //Code working for objects that implement Equals
            var IL_0020 = new[]
            {
                msil.Create(OpCodes.Ldloc_0),
                msil.Create(OpCodes.Ldloc_1),
                msil.Create(equalityReference.IsVirtual ? OpCodes.Callvirt : OpCodes.Call, equality),
                msil.Create(OpCodes.Ldc_I4_0),
                msil.Create(OpCodes.Ceq)
            };
            var IL_002a = new[]
            {
                msil.Create(OpCodes.Stloc_2),
                msil.Create(OpCodes.Ldloc_2),
                msil.Create(OpCodes.Brfalse_S, rtn),
                msil.Create(OpCodes.Nop)
            };

            return new[] {
                msil.Create(OpCodes.Nop),

                //V_0 = Property-Get
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Call, strategy.Property.GetMethod),
                msil.Create(OpCodes.Stloc_0),

                //Original Property-Set = value
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Ldarg_1),
                msil.Create(OpCodes.Call, originalSetMethod),

                //V_1 = Property-Get
                msil.Create(OpCodes.Nop),
                msil.Create(OpCodes.Ldarg_0),
                msil.Create(OpCodes.Call, strategy.Property.GetMethod),
                msil.Create(OpCodes.Stloc_1),

                //if (!V_0?.Equals(V_1) ?? (V_1 != null))
                msil.Create(OpCodes.Ldloc_0),
                msil.Create(OpCodes.Brtrue_S, IL_0020.First()),
                msil.Create(OpCodes.Ldloc_1),
                msil.Create(OpCodes.Ldnull),
                msil.Create(OpCodes.Cgt_Un),
                msil.Create(OpCodes.Br_S, IL_002a.First()),
            }
            .Concat(IL_0020)
            .Concat(IL_002a);
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