namespace PatchLanguage
{
    public class RunGlobals {
        public static void Print(object Message) {
            Console.Write(Message);
        }
        public static void PrintLine() {
            Console.WriteLine();
        }
        public static void PrintLine(object Message) {
            Console.WriteLine(Message);
        }
        public static Task<object> Evaluate(string Code) {
            List<Translation.Token> ParsedCode = Translation.Parse(Code);
            string TranslatedCode = Translation.Translate(ParsedCode);
            Microsoft.CodeAnalysis.Scripting.Script<object> CompiledScript = Translation.Compile(TranslatedCode);
            return Translation.Run(CompiledScript);
        }
    }

    public class Additions {
        public static class Maths {
            public static int Pow(int Num, int Power) {
                int Return = 1;
                while (Power != 0 ) { if ((Power & 1) == 1) { Return *= Num; } Num *= Num; Power >>= 1; }
                return Return;
            }
            public static long Pow(long Num, long Power) {
                long Return = 1;
                while (Power != 0) { if ((Power & 1) == 1) { Return *= Num; } Num *= Num; Power >>= 1; }
                return Return;
            }
            public static Int128 Pow(Int128 Num, Int128 Power) {
                Int128 Return = 1;
                while (Power != 0) { if ((Power & 1) == 1) { Return *= Num; } Num *= Num; Power >>= 1; }
                return Return;
            }
            public static float Pow(float Num, float Power) {
                return MathF.Pow(Num, Power);
            }
            public static double Pow(double Num, double Power) {
                return Math.Pow(Num, Power);
            }
            public static decimal Pow(decimal Num, decimal Power) {
                for (int i = 1; i < Power; i++) {
                    Num *= Num;
                }
                return Num;
            }
            public static System.Numerics.BigInteger Pow(System.Numerics.BigInteger Num, int Power) {
                return System.Numerics.BigInteger.Pow(Num, Power);
            }
        }
        public class Int32Range {
            public readonly int Start;
            public readonly int End;
            public readonly int Step;
            public Int32Range(int Start, int End, int Step) {
                this.Start = Start;
                this.End = End;
                this.Step = Step;
            }
            public Int32Range(int Start, int End) {
                this.Start = Start;
                this.End = End;
                this.Step = Start <= End ? 1 : -1;
            }
            public IEnumerator<int> GetEnumerator() {
                int i = Start;
                while (Step >= 0 ? (i >= Start && i <= End) : (i <= Start && i >= End)) {
                    yield return i;
                    i += Step;
                };
            }
        }
    }
    
    public static class Extensions {
        /// <returns>The Patch name of the object's type.</returns>
        public static string GetTypeName(this object Object) {
            Dictionary<Type, string> TypeNames = new() {
                {typeof(byte), "Int8"},
                {typeof(short), "Int16"},
                {typeof(int), "Int32"},
                {typeof(long), "Int64"},
                {typeof(Int128), "Int128"},
                {typeof(System.Numerics.BigInteger), "IntInf"},
                {typeof(Half), "Float16"},
                {typeof(float), "Float32"},
                {typeof(double), "Float64"},
                {typeof(decimal), "Float128"},
                {typeof(Type), "Type"},
                {typeof(bool), "Bool"},
                {typeof(string), "String"},
                {typeof(char), "Char"},
                {typeof(object), "Object"},
                {typeof(Task), "AsyncTask"},
            };
            if (Object == null) {
                return "Null";
            }
            TypeNames.TryGetValue(Object.GetType(), out string? TypeName);
            return TypeName ?? Object.GetType().Name;
        }
        public static T Convert<T>(this object Object) {
            try {
                if (typeof(T) == typeof(string)) {
                    return (T)(object)(Object.ToString() ?? "");
                }
                else if (typeof(T) == typeof(char)) {
                    string ObjectString = Object.ToString() ?? "";
                    if (ObjectString.Length == 1) {
                        return (T)(object)ObjectString[0];
                    }
                    throw new Exception($"Cannot convert '{ObjectString}' to type 'char' because its length is greater than one");
                }
                else if (typeof(T) == typeof(int)) {
                    return (T)(object)System.Convert.ToInt32(Object);
                }
            }
            catch {
            }
            throw new Exception($"Conversion to type '{typeof(T).Name}' is not supported");
        }
        /// <summary>
        /// Sets the boolean value to <see langword="true"/> if it's <see langword="false"/>, or to <see langword="false"/> if it's <see langword="true"/>.
        /// </summary>
        /// <param name="Boolean"></param>
        public static void Toggle(this ref bool Boolean) {
            Boolean = !Boolean;
        }
    }
}
