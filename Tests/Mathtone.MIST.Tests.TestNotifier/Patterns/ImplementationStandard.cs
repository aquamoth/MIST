using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Mathtone.MIST.TestNotifier.Patterns {

    [Notifier(NotificationMode.Implicit, NotificationStyle.OnSet)]
	public class OnSet_Misted {
		public string StringValue { get; set; }

		public int IntValue { get; set; }

		[NotifyTarget]
		protected void OnChange(string propertyName) {
			Console.WriteLine(propertyName);
		}
	}

    public class OnSet_Manually
    {
        string stringValue;
        int intValue;

        public int ChangeCount => Changes.Count;
        public List<string> Changes { get; } = new List<string>();

        public string StringValue
        {
            get { return stringValue; }
            set
            {
                stringValue = value;
                OnPropertyChanged("StringValue");
            }
        }

        public int IntValue
        {
            get { return intValue; }
            set
            {
                intValue = value;
                OnPropertyChanged("IntValue");
            }
        }


        protected void OnPropertyChanged(string propertyName)
        {
            //Console.WriteLine(propertyName);
            Changes.Add(propertyName);
        }
    }

    [Notifier(NotificationMode.Implicit, NotificationStyle.OnChange)]
	public class OnChange_Misted {
		public string StringValue { get; set; }

		public int IntValue { get; set; }

		[NotifyTarget]
		protected void OnChange(string propertyName) {
			Console.WriteLine(propertyName);
		}
	}

	public class OnChange_Manually {
		string stringValue;
		int intValue;

        public int ChangeCount => Changes.Count;
        public List<string> Changes { get; } = new List<string>();

        public string StringValue
        {
            get { return stringValue; }
            set
            {
                var V_0 = StringValue;
                SetStringValue(value);
                var V_1 = StringValue;
                if (!V_0?.Equals(V_1) ?? (V_1 != null))
                {
                    OnPropertyChanged("StringValue");
                }
            }
        }
        private void SetStringValue(string value) { stringValue = value; }

		public int IntValue {
			get { return intValue; }
			set {
				var V_0 = IntValue;
                SetIntValue(value);
                var V_1 = IntValue;
                if (!V_0.Equals(V_1)) {
					OnPropertyChanged("IntValue");
				}
			}
		}
        private void SetIntValue(int value) { intValue = value; }

        public int? NullableIntValue
        {
            get { return _nullableIntValue; }
            set
            {
                var V_0 = NullableIntValue;
                SetNullableIntValue(value);
                var V_1 = NullableIntValue;
                if (!V_0?.Equals(V_1) ?? (V_1 != null))
                {
                    OnPropertyChanged("NullableIntValue");
                }
            }
        }
        private void SetNullableIntValue(int? value) { _nullableIntValue = value; }
        int? _nullableIntValue;

        public Cases.RefObjectOverrideEquals TestValue
        {
            get { return _testValue; }
            set
            {
                var tValue = _testValue;
                _testValue = value;
                if (!tValue?.Equals(value) ?? (value != null))
                {
                    OnPropertyChanged("TestValue");
                }
            }
        }
        private Cases.RefObjectOverrideEquals _testValue;

        protected void OnPropertyChanged(string propertyName) {
            //Console.WriteLine(propertyName);
            Changes.Add(propertyName);
		}
	}

}