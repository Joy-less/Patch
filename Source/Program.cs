using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Scripting;

namespace PatchLanguage
{
    public static class Program {
        public static void Main(string[] Args) {
            if (Args.Length == 0) {
                Console.Write("You must right click a file containing Patch code and open it in Patch.");
                Console.ReadKey(true);
                Console.WriteLine();
                throw new Exception("No code input.");
            }
            string Code = File.ReadAllText(Args[0]);

            // Display information
            Console.WriteLine($"Patch v{Translation.Version}");
            Console.WriteLine($"by {Translation.Author}");
            Console.WriteLine();
            // Display code
            Console.ForegroundColor = ConsoleColor.DarkGray;
            Console.WriteLine(Code);
            Console.ResetColor();
            try {
                // Parse
                long TimeBeforeParse = DateTimeOffset.Now.ToUnixTimeMilliseconds();
                List<Translation.Token> ParsedCode = Translation.Parse(Code);
                Console.WriteLine($"Parsed in {(DateTimeOffset.Now.ToUnixTimeMilliseconds() - TimeBeforeParse) / 1000d} seconds.");
                // Display parsed code
                Console.ForegroundColor = ConsoleColor.DarkGray;
                foreach (Translation.Token Token in ParsedCode) {
                    Console.WriteLine($"{Token.Type}{(Token.Value.Length != 0 ? ":" + Token.Value : string.Empty)}");
                }
                Console.ResetColor();
                // Translate
                long TimeBeforeTranslate = DateTimeOffset.Now.ToUnixTimeMilliseconds();
                string TranslatedCode = Translation.Translate(ParsedCode);
                Console.WriteLine($"Translated in {(DateTimeOffset.Now.ToUnixTimeMilliseconds() - TimeBeforeTranslate) / 1000d} seconds.");
                // Display translated code
                Console.ForegroundColor = ConsoleColor.DarkGray;
                Console.WriteLine(TranslatedCode);
                Console.ResetColor();
                Console.Write("Press enter to compile and run.");
                Console.ReadLine();
                // Compile
                long TimeBeforeCompile = DateTimeOffset.Now.ToUnixTimeMilliseconds();
                Script<object> CompiledScript = Translation.Compile(TranslatedCode);
                Console.WriteLine($"Compiled in {(DateTimeOffset.Now.ToUnixTimeMilliseconds() - TimeBeforeCompile) / 1000d} seconds.");
                // Run
                Translation.Run(CompiledScript).Wait();
            }
            catch (Exception E) {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.WriteLine();
                Console.Write(E.Message);
                Console.ReadKey();
            }
        }
    }
}
