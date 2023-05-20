using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.CSharp.Scripting;

namespace PatchLanguage
{
    public class Translation {
        public const string Version = "1.1.1";
        public const string Author = "Joyless";

        public readonly static Dictionary<string, string> IdentifierAliases = new() {
            {"Int8", "System.Byte"},
            {"Int16", "System.Int16"},
            {"Int32", "System.Int32"},
            {"Int64", "System.Int64"},
            {"Int128", "System.Int128"},
            {"IntInf", "System.Numerics.BigInteger"},
            {"Float16", "System.Half"},
            {"Float32", "System.Single"},
            {"Float64", "System.Double"},
            {"Float128", "System.Decimal"},
            {"Type", "System.Type"},
            {"Bool", "bool"},
            {"String", "string"},
            {"Char", "char"},
            {"Null", "null"},
            {"Any", "dynamic"},
            {"Infer", "var"},
            {"Object", "object"},
            {"AsyncTask", "System.Threading.Tasks.Task"},
            {"List", "System.Collections.Generic.List"},
            {"Event", "PatchLanguage.Additions.Event"},

            {"True", "true"},
            {"False", "false"},
            {"Typeof", "typeof"},
            {"Sizeof", "sizeof"},
            {"Is", "is"},
            {"In", "in"},
            {"Nameof", "nameof"},
            {"New", "new"},
            {"This", "this"},
            {"Out", "out"},
            {"Async", "async"},
            {"Public", "public"},
            {"Private", "private"},
            {"PublicView", "public_view"},
            {"Static", "static"},
            {"Sealed", "sealed"},
            {"Await", "await"},
            {"Break", "break"},
            {"Continue", "continue"},
            {"Return", "return"},
            {"Throw", "throw"},

            {"&&", "And"},
            {"||", "Or"},
            {"&|", "Xor"},
            {"!", "Not"}
        };
        private readonly static string[] LogicalOperators = new[] {"&&", "||", "&|", "??", "!"};
        private readonly static string[] KeywordsStartingBlock = new[] {"If", "Task", "Class", "Struct", "Foreach", "While", "Match", "Try"};
        private readonly static string[] KeywordsEndingBlock = new[] {"End", "Returns"};
        private readonly static string[] KeywordsStartingStructure = new[] {"Class", "Struct", "Task"};
        private readonly static string[] KeywordsStartingStructureExceptTask = new[] {"Class", "Struct"};
        private readonly static string[] KeywordsStartingIteration = new[] {"Foreach", "While"};
        private readonly static string[] Modifiers = new[] {"public", "private", "public_view", "protected", "static", "sealed", "async", "override", "virtual"};
        private readonly static string[] AccessModifiers = new[] {"public", "public_view", "private", "protected"};
        private readonly static TokenType[] TokenTypesThatCouldHaveValues = new[] {TokenType.Identifier, TokenType.Char, TokenType.String, TokenType.Literal, TokenType.EndParameters};
        private readonly static TokenType[] StatementSeparatorTokenTypes = new[] {TokenType.Newline, TokenType.Semicolon};
        private readonly static TokenType[] TokenTypesThatMayBeFollowedByAnIndex = new[] {TokenType.Char, TokenType.String, TokenType.Literal, TokenType.EndList, TokenType.EndParameters};
        private readonly static string[] KeywordsThatCannotBeIndexed = new[] {"in", "is", "out", "typeof", "sizeof", "true", "false", "this", "continue", "break", "await", "return", "new", "And", "Or", "Xor", "Not", "&&", "||", "&|", "!"};
        public enum TokenType {
            Newline,
            Semicolon,
            Identifier,
            StartParameters,
            EndParameters,
            NextParameter,
            StartCustomType,
            EndCustomType,
            NextCustomType,
            ArithmeticOperator,
            AssignmentOperator,
            ConditionalOperator,
            Literal,
            String,
            Char,
            StartClassOrStruct,
            StartTask,
            StartIteration,
            StartMatch,
            StartCase,
            StartTryOrCatchOrFinally,
            InheritsFrom,
            BeginningOfDestructor,
            NumberRangeSeparator,
            StartList,
            EndList,
            NextListItem,
            StartIndex,
            EndIndex,
            ElseInMatchCase
        }
        public class Token {
            public readonly TokenType Type;
            public string Value;
            public Token(TokenType Type, string Value = "") {
                this.Type = Type;
                this.Value = Value;
            }
        }

        /// <summary>Converts an identifier in PascalCase, lowerCamelCase, lower_snake_case or UPPER_SNAKE_CASE to PascalCase.</summary>
        private static string ToPascalCase(string Characters) {
            if (char.IsLower(Characters[0])) {
                Characters = char.ToUpper(Characters[0]) + Characters[1..];
            }
            string CasedCode = "";
            bool MakeNextCharaUppercase = false;
            foreach (char Chara in Characters) {
                if (Chara == '_') {
                    MakeNextCharaUppercase = true;
                }
                else {
                    CasedCode += MakeNextCharaUppercase ? char.ToUpper(Chara) : Chara;
                    MakeNextCharaUppercase = false;
                }
            }
            return CasedCode;
        }
        private static bool IsJapaneseCharacter(char Chara) {
            // Hiragana
            if (Chara >= 0x3040 && Chara <= 0x309F) {
                return true;
            }
            // Katakana
            else if (Chara >= 0x30A0 && Chara <= 0x30FF) {
                return true;
            }
            // Full-width Roman characters & Half-width katakana
            else if (Chara >= 0xFF00 && Chara <= 0xFFEF) {
                return true;
            }
            // Common & uncommon kanji
            else if (Chara >= 0x4E00 && Chara <= 0x9FAF) {
                return true;
            }
            return false;
        }
        private static bool IsValidNormalCharacterInIdentifier(char Chara) {
            return char.IsLetterOrDigit(Chara) || IsJapaneseCharacter(Chara);
        }
        private static bool IsValidCharacterToStartIdentifier(char Chara, char? NextChara) {
            return IsValidNormalCharacterInIdentifier(Chara) || Chara == '.' || LogicalOperators.Contains(Chara.ToString() + NextChara);
        }
        private static bool IsValidCharacterToContinueIdentifier(char Chara, char? LastChara) {
            return IsValidNormalCharacterInIdentifier(Chara) || Chara == '_' || Chara == '.' || LogicalOperators.Contains(LastChara + Chara.ToString());
        }
        private static bool IsValidIdentifier(string Word) {
            return LogicalOperators.Contains(Word) || (Word.All(x => IsValidNormalCharacterInIdentifier(x) || x == '_' || x == '.' || x == '-') && Word.Length >= 1 && (IsValidNormalCharacterInIdentifier(Word[0]) || Word[0] == '.' || Word[0] == '-') && Word[^1] != '_');
        }
        private static void AssertValidIdentifier(string Word) {
            if (IsValidIdentifier(Word) == false) {
                throw new Exception($"Invalid identifier: '{Word}'");
            }
        }
        private static string ToIdentifier(string Word) {
            // Assert validity
            AssertValidIdentifier(Word);
            // Split apart by dot
            string Identifier = "";
            foreach (string Object in Word.Split('.')) {
                Identifier += ToPascalCase(Object) + ".";
            }
            if (Identifier.EndsWith('.')) {
                Identifier = Identifier[..^1];
            }
            // Convert identifier alias to C# identifier
            int IndexOfDot = Identifier.IndexOf('.');
            if (IndexOfDot == -1) {
                if (IdentifierAliases.TryGetValue(Identifier, out string? IdentifierAlias)) {
                    Identifier = IdentifierAlias;
                }
            }
            else {
                string FirstPartOfIdentifier = Identifier[..IndexOfDot];
                if (IdentifierAliases.TryGetValue(FirstPartOfIdentifier, out string? IdentifierAlias)) {
                    Identifier = IdentifierAlias + Identifier[IndexOfDot..];
                }
            }
            return Identifier;
        }
        private static int FindEndOfStatement(List<Token> ParsedCode, int FromPosition) {
            for (int i = FromPosition; i < ParsedCode.Count; i++) {
                if (StatementSeparatorTokenTypes.Contains(ParsedCode[i].Type)) {
                    return i;
                }
            }
            return -1;
        }
        private class PatchStructure {
            public Stack<string> ClassTree;
            public string StructureName;
            public string StructureType;
            public bool PatchOver;
            public PatchStructure(Stack<string> ClassTree, string StructureName, string StructureType, bool PatchOver) {
                this.ClassTree = ClassTree;
                this.StructureName = StructureName;
                this.StructureType = StructureType;
                this.PatchOver = PatchOver;
            }
        }
        private class BuildingStructureTokens {
            public string StructureName;
            public string StructureType;
            public List<Token> TokensInStructure;
            public bool PatchOver;
            public BuildingStructureTokens(string StructureName, string StructureType, List<Token> TokensInStructure, bool PatchOver) {
                this.StructureName = StructureName;
                this.StructureType = StructureType;
                this.TokensInStructure = TokensInStructure;
                this.PatchOver = PatchOver;
            }
        }
        
        public static List<Token> Parse(string Code) {
            try {
                List<Token> ParsedCode = new();
                Code = Code.Replace("\r", "");
                Code += "\n"; // Ensure semicolons are added correctly

                bool BuildingString = false; string CurrentString = "";
                bool BuildingChar = false; string CurrentChar = "";
                bool BuildingLiteral = false; string CurrentLiteral = "";
                bool BuildingIdentifier = false; string CurrentIdentifier = "";
                bool BuildingComment = false;
                bool BuildingMultiLineComment = false;
                int BuildingParameters = 0;
                int BuildingCustomType = 0;
                int BuildingList = 0;
                int BuildingIndex = 0;
                bool BuildingListNotIndex = false;
                for (int CurrentCharacterPosition = 0; CurrentCharacterPosition < Code.Length; CurrentCharacterPosition++) {
                    char? LastLastCharacter = (CurrentCharacterPosition - 2 >= 0) ? Code[CurrentCharacterPosition - 2] : null;
                    char? LastCharacter = (CurrentCharacterPosition - 1 >= 0) ? Code[CurrentCharacterPosition - 1] : null;
                    char CurrentCharacter = Code[CurrentCharacterPosition];
                    char? NextCharacter = (CurrentCharacterPosition + 1 < Code.Length) ? Code[CurrentCharacterPosition + 1] : null;
                    char? NextNextCharacter = (CurrentCharacterPosition + 2 < Code.Length) ? Code[CurrentCharacterPosition + 2] : null;

                    bool DefaultCheck() {
                        // Start parameters
                        if (CurrentCharacter == '(') {
                            BuildingParameters++;
                            ParsedCode.Add(new Token(TokenType.StartParameters));
                        }
                        // Literal
                        else if (CurrentCharacter == '`') {
                            BuildingLiteral = true;
                        }
                        // String
                        else if (CurrentCharacter == '"') {
                            BuildingString = true;
                        }
                        // Char
                        else if (CurrentCharacter == '\'') {
                            BuildingChar = true;
                        }
                        // Multi-line Comment
                        else if (CurrentCharacter == '#' && NextCharacter == '#' && NextNextCharacter == '#') {
                            BuildingMultiLineComment = true;
                            CurrentCharacterPosition += 2;
                        }
                        // Comment
                        else if (CurrentCharacter == '#') {
                            BuildingComment = true;
                        }
                        // NewLine
                        else if (CurrentCharacter == '\n') {
                            ParsedCode.Add(new Token(TokenType.Newline));
                        }
                        // Semicolon
                        else if (CurrentCharacter == ';') {
                            ParsedCode.Add(new Token(TokenType.Semicolon));
                        }
                        // Start custom type
                        else if (CurrentCharacter == '{') {
                            ParsedCode.Add(new Token(TokenType.StartCustomType));
                            BuildingCustomType = 1;
                        }
                        // Start list / index
                        else if (CurrentCharacter == '[') {
                            if ((ParsedCode[^1].Type == TokenType.Identifier && KeywordsThatCannotBeIndexed.Contains(ParsedCode[^1].Value) == false) || TokenTypesThatMayBeFollowedByAnIndex.Contains(ParsedCode[^1].Type)) {
                                ParsedCode.Add(new Token(TokenType.StartIndex));
                                BuildingIndex++;
                                BuildingListNotIndex = false;
                            }
                            else {
                                ParsedCode.Add(new Token(TokenType.StartList));
                                BuildingList++;
                                BuildingListNotIndex = true;
                            }
                        }
                        // Number range
                        else if (CurrentCharacter == '~' && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.NumberRangeSeparator));
                        }
                        // Inherits from
                        else if (CurrentCharacter == ':' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.InheritsFrom));
                        }
                        /* Patch asterisks are a cancelled feature.
                        // Patch asterisks
                        else if (CurrentCharacter == '*' && ParsedCode.Count >= 1 && StatementSeparatorTokens.Contains(ParsedCode[^1].Type)) {
                            BuildingPatchAsterisks = true;
                            CurrentPatchAsterisks += "*";
                        }*/
                        // Conditional Operator
                        else if ("<>".Contains(CurrentCharacter) && NextCharacter != '=' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.ConditionalOperator, CurrentCharacter.ToString()));
                        }
                        // Conditional Operator (two characters)
                        else if ("=!<>".Contains(CurrentCharacter) && NextCharacter == '=' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.ConditionalOperator, CurrentCharacter.ToString() + NextCharacter));
                            CurrentCharacterPosition++;
                        }
                        // Assignment Operator
                        else if (CurrentCharacter == '=' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.AssignmentOperator, CurrentCharacter.ToString()));
                        }
                        // Assignment Operator (two characters)
                        else if ("+-*/%^".Contains(CurrentCharacter) && NextCharacter == '=' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.AssignmentOperator, CurrentCharacter.ToString() + NextCharacter));
                            CurrentCharacterPosition++;
                        }
                        /* Unfortunately, ??= is only introduced in C# 8.0.
                        // Assignment Operator (three characters)
                        else if (CurrentCharacter == '?' && NextCharacter == '?' && NextNextCharacter == '=' && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.AssignmentOperator, CurrentCharacter.ToString() + NextCharacter + NextNextCharacter));
                            CurrentCharacterPosition += 2;
                        }*/
                        // Arithmetic Operator
                        else if ("+-*/%^".Contains(CurrentCharacter) && ParsedCode.Count >= 1 && TokenTypesThatCouldHaveValues.Contains(ParsedCode[^1].Type)) {
                            ParsedCode.Add(new Token(TokenType.ArithmeticOperator, CurrentCharacter.ToString()));
                        }
                        // Start identifier
                        else if (IsValidCharacterToStartIdentifier(CurrentCharacter, NextCharacter) || CurrentCharacter == '-') {
                            BuildingIdentifier = true;
                            CurrentIdentifier += CurrentCharacter;
                        }
                        // Logical operator - not
                        else if (CurrentCharacter == '!') {
                            ParsedCode.Add(new Token(TokenType.Identifier, IdentifierAliases["!"]));
                        }
                        // Whitespace
                        else if (char.IsWhiteSpace(CurrentCharacter)) {
                        }
                        // Invalid / other
                        else {
                            return false;
                        }
                        return true;
                    }
                    
                    // Multi-line Comment
                    if (BuildingMultiLineComment == true) {
                        if (LastLastCharacter == '#' && LastCharacter == '#' && CurrentCharacter == '#') {
                            BuildingMultiLineComment = false;
                        }
                        else if (CurrentCharacter == '\n') {
                            ParsedCode.Add(new Token(TokenType.Newline)); // Preserve line numbers
                        }
                    }
                    // Comment
                    else if (BuildingComment == true) {
                        if (CurrentCharacter == '\n') {
                            BuildingComment = false;
                            ParsedCode.Add(new Token(TokenType.Newline));
                        }
                    }
                    /* Patch asterisks are a cancelled feature.
                    // Patch asterisks
                    else if (BuildingPatchAsterisks == true) {
                        if (CurrentCharacter == '*') {
                            CurrentPatchAsterisks += "*";
                        }
                        else {
                            BuildingPatchAsterisks = false;
                            ParsedCode.Add(new Token(TokenType.PatchAsterisks, CurrentPatchAsterisks));
                            CurrentPatchAsterisks = "";

                            CurrentCharacterPosition--;
                            continue;
                        }
                    }*/
                    // Literal
                    else if (BuildingLiteral == true) {
                        if (CurrentCharacter == '`' && LastCharacter != '\\') {
                            BuildingLiteral = false;
                            ParsedCode.Add(new Token(TokenType.Literal, CurrentLiteral));
                            CurrentLiteral = "";
                        }
                        else {
                            CurrentLiteral += CurrentCharacter;
                        }
                    }
                    // String
                    else if (BuildingString == true) {
                        if (CurrentCharacter == '"' && LastCharacter != '\\') {
                            BuildingString = false;
                            ParsedCode.Add(new Token(TokenType.String, CurrentString));
                            CurrentString = "";
                        }
                        else {
                            CurrentString += CurrentCharacter;
                        }
                    }
                    // Char
                    else if (BuildingChar == true) {
                        if (CurrentCharacter == '\'' && LastCharacter != '\\') {
                            BuildingChar = false;
                            ParsedCode.Add(new Token(TokenType.Char, CurrentChar));
                            CurrentChar = "";
                        }
                        else {
                            CurrentChar += CurrentCharacter;
                        }
                    }
                    // Identifier
                    else if (BuildingIdentifier == true) {
                        if (IsValidCharacterToContinueIdentifier(CurrentCharacter, LastCharacter)) {
                            CurrentIdentifier += CurrentCharacter;
                        }
                        else {
                            BuildingIdentifier = false;
                            ParsedCode.Add(new Token(TokenType.Identifier, ToIdentifier(CurrentIdentifier)));
                            CurrentIdentifier = "";

                            CurrentCharacterPosition--;
                            continue;
                        }
                    }
                    // Custom type
                    else if (BuildingCustomType >= 1) {
                        if (CurrentCharacter == ',') {
                            ParsedCode.Add(new Token(TokenType.NextCustomType));
                        }
                        else if (CurrentCharacter == '{') {
                            BuildingCustomType++;
                            ParsedCode.Add(new Token(TokenType.StartCustomType));
                        }
                        else if (CurrentCharacter == '}') {
                            BuildingCustomType--;
                            ParsedCode.Add(new Token(TokenType.EndCustomType));
                        }
                        else if (DefaultCheck() == true) {
                        }
                    }
                    // List / Index
                    else if (BuildingList >= 1 || BuildingIndex >= 1) {
                        if (CurrentCharacter == ',') {
                            if (BuildingIndex >= 1 && BuildingListNotIndex == false) {
                                throw new Exception("Cannot have a 2-dimensional index. Did you mean to use a list inside a list?");
                            }
                            ParsedCode.Add(new Token(TokenType.NextListItem));
                        }
                        else if (CurrentCharacter == ']') {
                            if (BuildingList >= 1 && BuildingListNotIndex == true) {
                                BuildingList--;
                                ParsedCode.Add(new Token(TokenType.EndList));
                                if (BuildingList == 0 && BuildingIndex >= 1) {
                                    BuildingListNotIndex = false;
                                }
                            }
                            else if (BuildingIndex >= 1 && BuildingListNotIndex == false) {
                                BuildingIndex--;
                                ParsedCode.Add(new Token(TokenType.EndIndex));
                                if (BuildingIndex == 0 && BuildingList >= 1) {
                                    BuildingListNotIndex = true;
                                }
                            }
                        }
                        else if (DefaultCheck() == true) {
                        }
                    }
                    // Parameters
                    else if (BuildingParameters >= 1) {
                        if (DefaultCheck() == true) {
                        }
                        else if (CurrentCharacter == ',') {
                            ParsedCode.Add(new Token(TokenType.NextParameter));
                        }
                        else if (CurrentCharacter == ')') {
                            BuildingParameters--;
                            ParsedCode.Add(new Token(TokenType.EndParameters));
                        }
                        else {
                            throw new Exception($"Character not valid in parameter: '{CurrentCharacter}'");
                        }
                    }
                    // Other
                    else {
                        if (DefaultCheck() == false) {
                            throw new Exception($"Character not valid: '{CurrentCharacter}'");
                        }
                    }
                }

                // By this point, the parser has built the entire token list.
                // Below is extra processing done on the token list.

                // Replace a { b } with a{b}
                int NewIdentifierStartPosition = -1;
                string NewIdentifier = "";
                int NewIdentifierCustomTypeDepth = 0;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && (NewIdentifierStartPosition == -1 || NewIdentifierCustomTypeDepth > 0)) {
                        if (NewIdentifierStartPosition == -1) {
                            NewIdentifierStartPosition = i;
                        }
                        NewIdentifier += ParsedCode[i].Value;
                    }
                    else if (ParsedCode[i].Type == TokenType.StartCustomType) {
                        NewIdentifierCustomTypeDepth++;
                        NewIdentifier += "{";
                    }
                    else if (ParsedCode[i].Type == TokenType.EndCustomType) {
                        NewIdentifierCustomTypeDepth--;
                        NewIdentifier += "}";
                    }
                    else if (ParsedCode[i].Type == TokenType.NextCustomType) {
                        NewIdentifier += ", ";
                    }
                    else if (NewIdentifierStartPosition != -1 && NewIdentifierCustomTypeDepth == 0) {
                        // Remove the old identifiers and custom type markers
                        ParsedCode.RemoveRange(NewIdentifierStartPosition, i - NewIdentifierStartPosition);
                        // Insert the new identifier with custom type markers included
                        ParsedCode.Insert(NewIdentifierStartPosition, new Token(TokenType.Identifier, NewIdentifier));
                        i = NewIdentifierStartPosition;
                        NewIdentifierStartPosition = -1;
                        NewIdentifier = "";
                    }
                }

                // Replace values too large with parsed alternatives
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        string Value = ParsedCode[i].Value;
                        if (System.Numerics.BigInteger.TryParse(Value, out _)) {
                            if (Int128.TryParse(Value, out _) == false) {
                                ParsedCode[i].Value = $"System.Numerics.BigInteger.Parse(\"{Value}\")";
                            }
                            else if (long.TryParse(Value, out _) == false) {
                                ParsedCode[i].Value = $"System.Int128.Parse(\"{Value}\")";
                            }
                        }
                        /*else if (decimal.TryParse(Value, out _)) {
                            if (double.TryParse(Value, out _) == false) {
                                ParsedCode[i].Value = $"System.Double.Parse(\"{Value}\")";
                            }
                        }*/
                    }
                }

                // Replace "x if y" with "if y then x end"
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == "If") {
                        bool IsSubsequentIfStatement = true;
                        List<Token> SubsequentIfStatementCondition = new();
                        // Check whether if statement is subsequent, and if so then get & remove words after and including the if statement
                        for (int i2 = i + 1; i2 < ParsedCode.Count; i2++) {
                            if (ParsedCode[i2].Type == TokenType.Identifier && ParsedCode[i2].Value == "Then") {
                                IsSubsequentIfStatement = false;
                                break;
                            }
                            else if (StatementSeparatorTokenTypes.Contains(ParsedCode[i2].Type)) {
                                for (int i3 = i + 1; i3 < i2; i3++) {
                                    SubsequentIfStatementCondition.Add(ParsedCode[i3]);
                                }
                                ParsedCode.RemoveRange(i, SubsequentIfStatementCondition.Count + 1);

                                break;
                            }
                        }
                        // Replace subsequent if statement with normal if statement
                        if (IsSubsequentIfStatement == true) {
                            for (int i2 = i - 1; i2 > 0; i2--) {
                                if (ParsedCode[i2].Type == TokenType.Newline || ParsedCode[i2].Type == TokenType.Semicolon) {
                                    List<Token> ValuesToInsert = new() {
                                        new Token(TokenType.Identifier, "If")
                                    };
                                    foreach (Token Condition in SubsequentIfStatementCondition) {
                                        ValuesToInsert.Add(Condition);
                                    }
                                    ValuesToInsert.Add(new Token(TokenType.Identifier, "Then"));

                                    ParsedCode.Insert(i, new Token(TokenType.Identifier, "End"));
                                    ParsedCode.InsertRange(i2 + 1, ValuesToInsert);

                                    i += ValuesToInsert.Count;
                                    break;
                                }
                            }
                        }
                    }
                }

                // Check that all end blocks correlate to a start block
                int CheckDepth = 0;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        if (KeywordsStartingBlock.Contains(ParsedCode[i].Value)) {
                            CheckDepth++;
                        }
                        else if (KeywordsEndingBlock.Contains(ParsedCode[i].Value)) {
                            CheckDepth--;
                        }
                    }
                }
                if (CheckDepth > 0) {
                    throw new Exception("Start of block is not ended. Did you mean to include an 'end' keyword?");
                }
                else if (CheckDepth < 0) {
                    throw new Exception("End of block does not correlate to start of a block. Did you include an extra 'end' keyword?");
                }

                // Replace "a ^ b" with "Maths.Pow(a, b)"
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.ArithmeticOperator && ParsedCode[i].Value == "^") {
                        Token IdentifierOne = ParsedCode[i - 1];
                        Token IdentifierTwo = ParsedCode[i + 1];
                        ParsedCode.RemoveRange(i - 1, 3);
                        ParsedCode.InsertRange(i - 1, new[] {
                            new Token(TokenType.Identifier, "PatchLanguage.Maths.Pow"),
                            new Token(TokenType.StartParameters),
                            IdentifierOne,
                            new Token(TokenType.NextParameter),
                            IdentifierTwo,
                            new Token(TokenType.EndParameters)
                        });
                    }
                }

                // Replace a~b~c with new Int32Range(a, b, c)
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.NumberRangeSeparator) {
                        Token IdentifierA = ParsedCode[i - 1];
                        if (ParsedCode[i + 1].Type != TokenType.Identifier) {
                            throw new Exception("Number range separators must be followed by an identifier.");
                        }
                        Token IdentifierB = ParsedCode[i + 1];
                        ParsedCode.RemoveRange(i - 1, 3);
                        i--;

                        Token Step;
                        if (ParsedCode[i].Type == TokenType.NumberRangeSeparator) {
                            if (ParsedCode[i + 1].Type != TokenType.Identifier) {
                                throw new Exception("Number range separators must be followed by an identifier.");
                            }
                            Step = ParsedCode[i + 1];
                            ParsedCode.RemoveRange(i, 2);
                        }
                        else {
                            Step = new Token(TokenType.Identifier, "1");
                        }

                        ParsedCode.InsertRange(i, new[] {
                            new Token(TokenType.Identifier, "new"),
                            new Token(TokenType.Identifier, "Int32Range"),
                            new Token(TokenType.StartParameters),
                            IdentifierA,
                            new Token(TokenType.NextParameter),
                            IdentifierB,
                            new Token(TokenType.NextParameter),
                            Step,
                            new Token(TokenType.EndParameters),
                        });
                    }
                }

                // Replace '[a, b, c]' with 'new List<dynamic>() {a, b, c}'
                // * Allow an optional data type for the list (e.g. [string a, b, c])
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.StartList) {
                        string ListDataType = "dynamic";
                        if (i + 2 < ParsedCode.Count && ParsedCode[i + 1].Type == TokenType.Identifier && ParsedCode[i + 2].Type != TokenType.NextListItem) {
                            ListDataType = ParsedCode[i + 1].Value;
                            ParsedCode.RemoveAt(i + 1);
                        }

                        int EndOfListPosition = -1;
                        for (int i2 = i + 1; i2 < ParsedCode.Count; i2++) {
                            if (ParsedCode[i2].Type == TokenType.EndList) {
                                EndOfListPosition = i2;
                                break;
                            }
                        }
                        if (EndOfListPosition == -1) {
                            throw new Exception("A square bracket to start a list must be followed by a closing square bracket to end the list.");
                        }
                        ParsedCode.InsertRange(i, new[] {
                            new Token(TokenType.Identifier, "new"),
                            new Token(TokenType.Identifier, "System.Collections.Generic.List{" + ListDataType + "}"),
                            new Token(TokenType.StartParameters),
                            new Token(TokenType.EndParameters)
                        });
                        EndOfListPosition += 4;
                        i = EndOfListPosition + 1;
                    }
                }

                // Insert StartIteration after foreach and while loops
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && KeywordsStartingIteration.Contains(ParsedCode[i].Value)) {
                        int PositionToSearch = -1;
                        if (ParsedCode[i].Value == "Foreach") {
                            for (int i2 = i + 1; i2 < ParsedCode.Count; i2++) {
                                if (ParsedCode[i2].Type == TokenType.Identifier) {
                                    if (ParsedCode[i2].Value == "in") {
                                        PositionToSearch = i2;
                                        break;
                                    }
                                }
                                else {
                                    throw new Exception($"{ParsedCode[i].Value} statements must be followed by \"in\".");
                                }
                            }
                            if (PositionToSearch + 1 >= ParsedCode.Count || ParsedCode[PositionToSearch + 1].Type != TokenType.Identifier) {
                                throw new Exception("In statements must be followed by a value.");
                            }
                        }
                        else {
                            PositionToSearch = i;
                        }
                        ParsedCode.Insert(FindEndOfStatement(ParsedCode, PositionToSearch), new Token(TokenType.StartIteration));
                    }
                }

                // Insert StartMatch and StartCase in match case statements, also add breaks
                int DepthForMatch = 0;
                int DepthWithinMatch = 0;
                bool MatchIsEmpty = true;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        if (ParsedCode[i].Value == "Match") {
                            DepthForMatch++;
                            DepthWithinMatch = 1;
                            // Insert StartMatch at start of match statement
                            int StartMatchPosition = FindEndOfStatement(ParsedCode, i + 1);
                            if (StartMatchPosition == -1) {
                                throw new Exception("Match statements must be followed by a newline or semicolon.");
                            }
                            ParsedCode.Insert(StartMatchPosition, new Token(TokenType.StartMatch));
                            i = StartMatchPosition + 1;
                        }
                        else if (ParsedCode[i].Value == "Case" && DepthWithinMatch >= 1) {
                            MatchIsEmpty = false;
                            // Insert StartCase at start of case statement
                            int StartCasePosition = FindEndOfStatement(ParsedCode, i + 1);
                            if (StartCasePosition == -1) {
                                throw new Exception("Case statements must be followed by a newline or semicolon.");
                            }
                            ParsedCode.Insert(StartCasePosition, new Token(TokenType.StartCase));
                            i = StartCasePosition + 1;
                            // Insert break if case statement is empty
                            bool CaseIsEmpty = true;
                            int NestedMatchDepth = 0;
                            for (int i2 = i; i2 < ParsedCode.Count; i2++) {
                                if (StatementSeparatorTokenTypes.Contains(ParsedCode[i2].Type) == false) {
                                    if (NestedMatchDepth == 0 && ParsedCode[i2].Type == TokenType.Identifier && (ParsedCode[i2].Value == "Case" || ParsedCode[i2].Value == "End" || ParsedCode[i2].Value == "Else")) {
                                        if (CaseIsEmpty == false && ParsedCode[i2].Value != "End") {
                                            ParsedCode.InsertRange(i2, new[] {
                                                new Token(TokenType.Identifier, "break"),
                                                new Token(TokenType.Semicolon)
                                            });
                                        }
                                        break;
                                    }
                                    else if (ParsedCode[i2].Type == TokenType.Identifier && ParsedCode[i2].Value == "Match") {
                                        CaseIsEmpty = false;
                                        NestedMatchDepth++;
                                    }
                                    else if (NestedMatchDepth >= 1) {
                                        if (KeywordsStartingBlock.Contains(ParsedCode[i2].Value)) {
                                            NestedMatchDepth++;
                                        }
                                        else if (KeywordsEndingBlock.Contains(ParsedCode[i2].Value)) {
                                            NestedMatchDepth--;
                                        }
                                    }
                                    else {
                                        CaseIsEmpty = false;
                                    }
                                }
                            }
                        }
                        else if (ParsedCode[i].Value == "Else" && DepthWithinMatch >= 1) {
                            MatchIsEmpty = false;
                            ParsedCode[i] = new Token(TokenType.ElseInMatchCase);
                            ParsedCode.Insert(i + 1, new Token(TokenType.StartCase));
                            i++;
                        }
                        else if (KeywordsStartingBlock.Contains(ParsedCode[i].Value)) {
                            DepthForMatch++;
                        }
                        else if (KeywordsEndingBlock.Contains(ParsedCode[i].Value)) {
                            // Insert break before end if match case is not empty
                            if (DepthForMatch >= 1) {
                                if (MatchIsEmpty == false && ParsedCode[i].Value == "End") {
                                    ParsedCode.InsertRange(i, new[] {
                                        new Token(TokenType.Identifier, "break"),
                                        new Token(TokenType.Semicolon)
                                    });
                                    i += 2;
                                }
                                MatchIsEmpty = true;
                            }
                            // Reduce match depth
                            DepthForMatch--;
                            if (DepthWithinMatch >= 1) {
                                DepthWithinMatch--;
                            }
                        }
                    }
                }

                // Replace and, or, xor, not with &&, ||, ^, !
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        switch (ParsedCode[i].Value) {
                            case "And":
                                ParsedCode[i].Value = "&&";
                                break;
                            case "Or":
                                ParsedCode[i].Value = "||";
                                break;
                            case "Xor":
                                ParsedCode[i].Value = "^";
                                break;
                            case "Not":
                                ParsedCode[i].Value = "!";
                                break;
                        }
                    }
                }

                /* Because Microsoft.CodeAnalysis.CSharp.Scripting wraps code into a class, namespaces are disallowed.
                 * To have a more structured approach, CS-Script may be preferable.
                 * The use-cases of Patch are small enough scope to not need namespaces.
                 * The drawback of using CS-Script is that top-level statements would have to be wrapped into a class manually by Patch.
                // Add StartClassOrStructOrNameSpace to namespace
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == "Namespace") {
                        if (i + 2 < ParsedCode.Count && ParsedCode[i + 1].Type == TokenType.Identifier) {
                            ParsedCode.Insert(i + 2, new Token(TokenType.StartClassOrStructOrNamespace));
                        }
                        else {
                            throw new Exception("Namespace keyword must be followed by an identifier.");
                        }
                    }
                }*/

                // Add StartClassOrStruct and StartTask after class/struct and task
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && KeywordsStartingStructure.Contains(ParsedCode[i].Value)) {
                        if (i + 1 < ParsedCode.Count && ParsedCode[i + 1].Type == TokenType.Identifier) {
                            // Add StartClassOrStruct after class/struct
                            if (KeywordsStartingStructureExceptTask.Contains(ParsedCode[i].Value)) {
                                int PositionToInsert = i + 2;
                                if (ParsedCode[PositionToInsert].Type == TokenType.InheritsFrom) {
                                    if (PositionToInsert + 2 < ParsedCode.Count && ParsedCode[PositionToInsert + 1].Type != TokenType.Identifier) {
                                        throw new Exception($"{ParsedCode[i].Value} {ParsedCode[i + 1].Value} must inherit from a valid identifier.");
                                    }
                                    PositionToInsert += 2;
                                }
                                ParsedCode.Insert(PositionToInsert, new Token(TokenType.StartClassOrStruct));
                            }
                            // Add StartTask after task
                            else {
                                // Get end of task identifier or parameters
                                int EndOfTaskIdentifier = 0;
                                bool AllowAnotherIdentifier = true;
                                bool TaskHasParameters = false;
                                for (int i2 = i + 1; i2 < ParsedCode.Count; i2++) {
                                    if (TaskHasParameters == false) {
                                        if (ParsedCode[i2].Type == TokenType.Identifier && AllowAnotherIdentifier == true) {
                                            AllowAnotherIdentifier = false;
                                        }
                                        else if (ParsedCode[i2].Type == TokenType.StartCustomType) {
                                            AllowAnotherIdentifier = true;
                                        }
                                        else if (ParsedCode[i2].Type == TokenType.Literal) {
                                        }
                                        else if (ParsedCode[i2].Type == TokenType.StartParameters) {
                                            TaskHasParameters = true;
                                        }
                                        else {
                                            EndOfTaskIdentifier = i2;
                                            break;
                                        }
                                    }
                                    else if (TaskHasParameters == true) {
                                        if (ParsedCode[i2].Type == TokenType.EndParameters) {
                                            EndOfTaskIdentifier = i2 + 1;
                                            break;
                                        }
                                    }
                                }
                                ParsedCode.Insert(EndOfTaskIdentifier, new Token(TokenType.StartTask));
                            }
                        }
                        else {
                            throw new Exception($"{ParsedCode[i].Value} keyword must be followed by a valid identifier");
                        }
                    }
                }

                /*
                patch class A
                    int D
                    class C
                        bool E
                        class H
                            
                        end
                    end
                    patch class B
                        char F
                    end
                    int G
                end

                Generates:

                {new PatchStructure(new List<string> {}, A, class, true), new List<Token> {int D, int G}}
                {new PatchStructure(new List<string> {A}, C, class, false), new List<Token> {bool E, class H, end}}
                {new PatchStructure(new List<string> {A}, B, class, true), new List<Token> {char F}}
                */

                // Get patches
                Dictionary<PatchStructure, List<Token>> StructuresToPatch = new();
                Stack<string> CurrentPatchClassTree = new();
                int PatchWithinClassDepth = 0;
                Stack<BuildingStructureTokens> StructureTokens = new();
                int PatchNotInsertDepth = 0;
                bool IsPatchStatementBefore = false;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    Token CurrentToken = ParsedCode[i];
                    Token? NextToken = i + 1 < ParsedCode.Count ? ParsedCode[i + 1] : null;

                    if (CurrentToken.Type == TokenType.Identifier && CurrentToken.Value == "Patch") {
                        IsPatchStatementBefore = true;
                        if (NextToken != null && NextToken.Type != TokenType.Identifier) {
                            throw new Exception("Patch statement must be followed by a valid structure type.");
                        }
                        continue;
                    }
                    else if (CurrentToken.Type != TokenType.Identifier) {
                        IsPatchStatementBefore = false;
                    }
                    // Start nested class / task
                    if (CurrentToken.Type == TokenType.Identifier && KeywordsStartingStructure.Contains(CurrentToken.Value)) {
                        // Get modifiers
                        List<Token> TokenList = new();
                        if (PatchNotInsertDepth >= 1 || IsPatchStatementBefore == true) {
                            while (ParsedCode[i - 1].Type == TokenType.Identifier && Modifiers.Contains(ParsedCode[i - 1].Value)) {
                                TokenList.Add(ParsedCode[i - 1]);
                                ParsedCode.RemoveAt(i - 1);
                                i--;
                            }
                            if (StructureTokens.Count >= 1) {
                                List<Token> CurrentTokensInStructure = StructureTokens.Peek().TokensInStructure;
                                while (CurrentTokensInStructure[^1].Type == TokenType.Identifier && Modifiers.Contains(CurrentTokensInStructure[^1].Value)) {
                                    TokenList.Add(CurrentTokensInStructure[^1]);
                                    CurrentTokensInStructure.RemoveAt(CurrentTokensInStructure.Count - 1);
                                }
                            }
                        }
                        // Check whether patching over or inserting new
                        bool PatchOver = false;
                        if (i - 1 >= 0 && ParsedCode[i - 1].Type == TokenType.Identifier && ParsedCode[i - 1].Value == "Patch") {
                            PatchOver = true;
                            PatchNotInsertDepth++;
                            ParsedCode.RemoveAt(i - 1);
                            i--;
                            if (i + 1 >= ParsedCode.Count || ParsedCode[i].Type != TokenType.Identifier || ParsedCode[i + 1].Type != TokenType.Identifier) {
                                throw new Exception("Patch statement must be followed by a valid type and identifier.");
                            }
                        }
                        string StructureName = ParsedCode[i + 1].Value;
                        string StructureType = ParsedCode[i].Value;
                        if (PatchNotInsertDepth >= 1) {
                            StructureTokens.Push(new BuildingStructureTokens(StructureName, StructureType, TokenList, PatchOver));
                        }
                    }

                    // Add token to tokens list and remove it from the parsed code list
                    if (StructureTokens.Count >= 1) {
                        StructureTokens.Peek().TokensInStructure.Add(CurrentToken);
                        ParsedCode.RemoveAt(i);
                        i--;

                        // Include return type if present
                        if (CurrentToken.Value == "Returns" && NextToken != null) {
                            StructureTokens.Peek().TokensInStructure.Add(NextToken);
                            i++;
                            ParsedCode.RemoveAt(i);
                            i--;
                        }
                    }

                    // Change depth / class tree
                    if (CurrentToken.Type == TokenType.Identifier && KeywordsStartingStructureExceptTask.Contains(CurrentToken.Value) && NextToken != null) {
                        CurrentPatchClassTree.Push(NextToken.Value);
                    }
                    else if (CurrentToken.Type == TokenType.Identifier && KeywordsStartingBlock.Contains(CurrentToken.Value)) {
                        PatchWithinClassDepth++;
                    }
                    else if (CurrentToken.Type == TokenType.Identifier && KeywordsEndingBlock.Contains(CurrentToken.Value)) {
                        if (PatchWithinClassDepth == 0) {
                            CurrentPatchClassTree.Pop();
                            // Remove End from tokens
                            if (StructureTokens.Count >= 1) {
                                List<Token> TokensInStructure = StructureTokens.Peek().TokensInStructure;
                                TokensInStructure.RemoveAt(TokensInStructure.Count - 1);
                            }
                        }
                        else {
                            PatchWithinClassDepth--;
                        }
                        if (PatchNotInsertDepth >= 1 && StructureTokens.TryPop(out BuildingStructureTokens? BuildingStructureTokens)) {
                            StructuresToPatch.Add(new PatchStructure(new Stack<string>(CurrentPatchClassTree), BuildingStructureTokens.StructureName, BuildingStructureTokens.StructureType,
                                BuildingStructureTokens.PatchOver), BuildingStructureTokens.TokensInStructure);
                            if (StructureTokens.Count == 0) {
                                PatchNotInsertDepth--;
                            }
                        }
                    }
                }

                // Apply patches
                bool AreStringStacksEqual(Stack<string> A, Stack<string> B)  {
                    return A.Count == B.Count && A.Where(B.Contains).Count() == A.Count;
                }
                List<PatchStructure> StructuresToPatchKeys = new(StructuresToPatch.Keys);
                for (int i3 = 0; i3 < StructuresToPatch.Count; i3++) {
                    PatchStructure StructureToPatch = StructuresToPatchKeys[i3];

                    string PatchStructureName = StructureToPatch.StructureName;
                    string PatchStructureType = StructureToPatch.StructureType;
                    bool PatchOver = StructureToPatch.PatchOver;
                    Stack<string> PatchClassTree = StructureToPatch.ClassTree;
                    List<Token> PatchTokens = StructuresToPatch[StructuresToPatchKeys[i3]];

                    Stack<string> OriginalClassTree = new();
                    int OriginalWithinClassDepth = 0;
                    bool PatchSuccessful = false;
                    for (int i = 0; i < ParsedCode.Count; i++) {
                        if (PatchOver == true && ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == PatchStructureType
                            && ParsedCode[i + 1].Type == TokenType.Identifier && ParsedCode[i + 1].Value == PatchStructureName && AreStringStacksEqual(OriginalClassTree, PatchClassTree)) {
                            // Console.WriteLine($"PATCH {PatchStructureType} {PatchStructureName}");
                            // Remove modifiers
                            for (int i2 = i - 1; i2 > 0; i2--) {
                                if (ParsedCode[i2].Type == TokenType.Identifier && Modifiers.Contains(ParsedCode[i2].Value)) {
                                    ParsedCode.RemoveAt(i2);
                                    i--;
                                }
                                else {
                                    break;
                                }
                            }
                            // Remove tokens up to StartClassOrStruct or StartTask
                            while (true) {
                                TokenType CurrentTokenType = ParsedCode[i].Type;
                                ParsedCode.RemoveAt(i);
                                if (CurrentTokenType == TokenType.StartClassOrStruct || CurrentTokenType == TokenType.StartTask) {
                                    break;
                                }
                            }
                            // If task, remove tokens up to End / Returns
                            // Because, unlike class patches which add to the class, task patches overwrite the task
                            if (PatchStructureType == "Task") {
                                while (true) {
                                    Token CurrentToken = ParsedCode[i];
                                    ParsedCode.RemoveAt(i);
                                    if (CurrentToken.Type == TokenType.Identifier) {
                                        if (CurrentToken.Value == "End") {
                                            break;
                                        }
                                        else if (CurrentToken.Value == "Returns" && ParsedCode[i].Type == TokenType.Identifier) {
                                            ParsedCode.RemoveAt(i);
                                            break;
                                        }
                                    }
                                }
                            }
                            // Patch over
                            int PatchDepth = 0;
                            int InsertOffset = 0;
                            foreach (Token PatchToken in PatchTokens) {
                                if (PatchToken.Type == TokenType.Identifier && PatchToken.Value == "Patch") {
                                    PatchDepth++;
                                }
                                else if (PatchDepth >= 1) {
                                    if (PatchToken.Type == TokenType.Identifier && KeywordsStartingBlock.Contains(PatchToken.Value)) {
                                        PatchDepth++;
                                    }
                                    else if (PatchToken.Type == TokenType.Identifier && KeywordsEndingBlock.Contains(PatchToken.Value)) {
                                        PatchDepth--;
                                    }
                                }
                                else if (PatchDepth == 0) {
                                    ParsedCode.Insert(i + InsertOffset, PatchToken);
                                    InsertOffset++;
                                }
                            }
                            PatchSuccessful = true;
                            break;
                        }
                        // Change depth / exit tree node
                        else if (ParsedCode[i].Type == TokenType.Identifier && KeywordsStartingStructureExceptTask.Contains(ParsedCode[i].Value) && ParsedCode[i + 1].Type == TokenType.Identifier) {
                            OriginalClassTree.Push(ParsedCode[i + 1].Value);
                        }
                        else if (ParsedCode[i].Type == TokenType.Identifier && KeywordsStartingBlock.Contains(ParsedCode[i].Value)) {
                            OriginalWithinClassDepth++;
                        }
                        else if (ParsedCode[i].Type == TokenType.Identifier && KeywordsEndingBlock.Contains(ParsedCode[i].Value)) {
                            if (OriginalWithinClassDepth == 0) {
                                // Insert new
                                if (PatchOver == false && AreStringStacksEqual(OriginalClassTree, PatchClassTree)) {
                                    // Console.WriteLine($"INSERT NEW {PatchStructureType} {PatchStructureName}");
                                    ParsedCode.InsertRange(i, PatchTokens);
                                    PatchSuccessful = true;
                                    break;
                                }
                                // Exit class tree node
                                if (OriginalClassTree.Count >= 1) {
                                    OriginalClassTree.Pop();
                                }
                            }
                            if (OriginalWithinClassDepth >= 1) {
                                OriginalWithinClassDepth--;
                            }
                        }
                    }
                    if (PatchSuccessful == false) {
                        // Override task in inherited class
                        if (PatchStructureType == "Task") {
                            PatchStructure NewStructureToPatch = new(PatchClassTree, PatchStructureName, PatchStructureType, false);
                            PatchTokens.Insert(0, new Token(TokenType.Identifier, "override"));
                            StructuresToPatch.Add(NewStructureToPatch, PatchTokens);
                            StructuresToPatchKeys.Add(NewStructureToPatch);
                        }
                        // Class to patch not found
                        else {
                            throw new Exception($"Could not find {PatchStructureType} {PatchStructureName} to patch.");
                        }
                    }
                }

                // Convert public_view to public and {get; protected set;}
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == "public_view") {
                        ParsedCode[i].Value = "public";
                        if (i + 3 >= ParsedCode.Count) {
                            throw new Exception("Public keyword must be followed by a data type, an identifier, an equals sign and a value.");
                        }
                        ParsedCode.Insert(i + 3, new Token(TokenType.Literal, "{get; protected set;}"));
                    }
                }

                // Set default access modifier
                // * Protected for members of classes
                // * Private for members of structs
                // * No access modifier for top-level members
                Stack<string> CurrentStructureTree = new();
                Stack<string> CurrentStructureNamesTree = new();
                int DepthWithinStructure = 0;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        if (CurrentStructureTree.Count >= 1 && StatementSeparatorTokenTypes.Contains(ParsedCode[i - 1].Type)
                            && KeywordsStartingStructureExceptTask.Contains(CurrentStructureTree.Peek()) && KeywordsEndingBlock.Contains(ParsedCode[i].Value) == false) {
                            // Get start of member
                            int StartOfMember = 0;
                            for (int i2 = i - 1; i2 >= 0; i2--) {
                                if (ParsedCode[i2].Type != TokenType.Identifier) {
                                    StartOfMember = i2 + 1;
                                    break;
                                }
                            }
                            // Get current modifiers
                            List<string> CurrentModifiers = new();
                            for (int i2 = StartOfMember; i2 < ParsedCode.Count; i2++) {
                                if (Modifiers.Contains(ParsedCode[i2].Value) == false) {
                                    break;
                                }
                                else {
                                    CurrentModifiers.Add(ParsedCode[i2].Value);
                                }
                            }
                            // Check if one of the modifiers is an access modifier
                            bool AlreadyHasAccessModifier = false;
                            foreach (string CurrentModifier in CurrentModifiers) {
                                if (AccessModifiers.Contains(CurrentModifier)) {
                                    AlreadyHasAccessModifier = true;
                                    break;
                                }
                            }
                            // Add default access modifier if an access modifier is not present
                            if (AlreadyHasAccessModifier == false) {
                                ParsedCode.Insert(StartOfMember, new Token(TokenType.Identifier, (CurrentStructureTree.Count >= 1 && CurrentStructureTree.Peek() == "Struct" ? "private" : "protected")));
                                i++;
                            }

                            // Extra: Convert constructors and destructors
                            for (int i2 = StartOfMember; i2 < ParsedCode.Count; i2++) {
                                if (ParsedCode[i2].Type == TokenType.Identifier) {
                                    if (ParsedCode[i2].Value == "Task") {
                                        string CurrentTaskName = ParsedCode[i2 + 1].Value;
                                        if (CurrentTaskName == "Construct" || CurrentTaskName == "Destruct") {
                                            // Create constructor/destructor
                                            ParsedCode.RemoveAt(i2 + 1);
                                            ParsedCode[i2].Value = CurrentStructureNamesTree.Peek();
                                            // Add () to constructor/destructor without brackets
                                            if (ParsedCode[i2 + 1].Type != TokenType.StartParameters) {
                                                ParsedCode.InsertRange(i2 + 1, new[] {new Token(TokenType.StartParameters), new Token(TokenType.EndParameters)});
                                            }
                                            // Add ~ to destructor
                                            if (CurrentTaskName == "Destruct") {
                                                ParsedCode.Insert(i2, new Token(TokenType.BeginningOfDestructor));
                                            }
                                            // Remove modifiers
                                            while (ParsedCode[i2 - 1].Type == TokenType.Identifier && Modifiers.Contains(ParsedCode[i2 - 1].Value)) {
                                                ParsedCode.RemoveAt(i2 - 1);
                                                i2--;
                                            }
                                            // Add the public modifier if constructor
                                            if (CurrentTaskName == "Construct") {
                                                ParsedCode.Insert(i2, new Token(TokenType.Identifier, "public"));
                                            }
                                            // Add constructor/destructor to tree (as it will be overlooked otherwise)
                                            CurrentStructureTree.Push("Task");
                                            CurrentStructureNamesTree.Push(CurrentTaskName);
                                        }
                                    }
                                }
                                else {
                                    break;
                                }
                            }
                        }

                        if (KeywordsStartingStructure.Contains(ParsedCode[i].Value)) {
                            DepthWithinStructure = 0;
                            CurrentStructureTree.Push(ParsedCode[i].Value);
                            CurrentStructureNamesTree.Push(ParsedCode[i + 1].Value);
                        }
                        else if (KeywordsStartingBlock.Contains(ParsedCode[i].Value)) {
                            DepthWithinStructure++;
                        }
                        else if (KeywordsEndingBlock.Contains(ParsedCode[i].Value)) {
                            if (DepthWithinStructure >= 1) {
                                DepthWithinStructure--;
                            }
                            else if (CurrentStructureTree.Count >= 1) {
                                CurrentStructureTree.Pop();
                                CurrentStructureNamesTree.Pop();
                            }
                        }
                    }
                }

                // Add a semicolon after anonymous tasks which are followed by a newline
                int DepthWithinAnonymousTask = 0;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        if (ParsedCode[i].Value == "Anon") {
                            DepthWithinAnonymousTask = 1;
                        }
                        else if (DepthWithinAnonymousTask >= 1) {
                            if (KeywordsStartingBlock.Contains(ParsedCode[i].Value)) {
                                DepthWithinAnonymousTask++;
                            }
                            else if (KeywordsEndingBlock.Contains(ParsedCode[i].Value)) {
                                if (DepthWithinAnonymousTask == 1 && ParsedCode[i + 1].Type == TokenType.Newline) {
                                    ParsedCode.Insert(i + 1, new Token(TokenType.Semicolon));
                                }
                                DepthWithinAnonymousTask--;
                            }
                        }
                    }
                }

                // Convert task to return type, add () if not present, and do other processing
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == "Task") {
                        // Check for invalid task syntax
                        if (ParsedCode[i + 1].Type != TokenType.Identifier)  {
                            throw new Exception("Task keyword must be followed by an identifier.");
                        }

                        ParsedCode[i].Value = "void";

                        // Get end of task identifier
                        int EndOfTaskIdentifier = i;
                        do {
                            EndOfTaskIdentifier++;
                        } while (ParsedCode[EndOfTaskIdentifier].Type != TokenType.StartTask);
                        // Add () to task without brackets
                        if (ParsedCode[EndOfTaskIdentifier - 1].Type != TokenType.EndParameters) {
                            ParsedCode.InsertRange(EndOfTaskIdentifier, new[] {new Token(TokenType.StartParameters), new Token(TokenType.EndParameters)});
                            EndOfTaskIdentifier += 2;
                        }

                        // Convert anonymous tasks into C# delegates
                        bool IsAnonymousTask = false;
                        if (ParsedCode[i + 1].Value == "Anon") {
                            IsAnonymousTask = true;
                            ParsedCode.RemoveAt(i + 1);
                            EndOfTaskIdentifier--;
                            ParsedCode.Insert(EndOfTaskIdentifier, new Token(TokenType.Literal, "=>"));
                            EndOfTaskIdentifier++;
                        }

                        // Convert void and returns to the return value and end
                        int Depth = 0;
                        for (int i2 = i + 1; i2 < ParsedCode.Count; i2++) {
                            if (Depth == 0 && ParsedCode[i2].Type == TokenType.Identifier) {
                                if (ParsedCode[i2].Value == "End") {
                                    break;
                                }
                                else if (ParsedCode[i2].Value == "Returns") {
                                    if (i2 + 1 < ParsedCode.Count && ParsedCode[i2 + 1].Type == TokenType.Identifier) {
                                        ParsedCode[i].Value = ParsedCode[i2 + 1].Value; // Change void to the data type
                                        ParsedCode.RemoveAt(i2 + 1); // Remove the data type after the returns keyword
                                        ParsedCode[i2].Value = "End"; // Replace the returns keyword with end
                                    }
                                    else {
                                        throw new Exception("Returns keyword must be followed by a data type.");
                                    }
                                    break;
                                }
                            }
                        }

                        // Check for async, override and private modifier
                        bool HasAsyncModifier = false;
                        bool HasOverrideModifier = false;
                        bool HasAccessModifierWhichIsNotPrivate = false;
                        for (int i2 = i - 1; i2 >= 0; i2--) {
                            if (ParsedCode[i2].Type == TokenType.Identifier && Modifiers.Contains(ParsedCode[i2].Value)) {
                                if (ParsedCode[i2].Value == "async") {
                                    HasAsyncModifier = true;
                                }
                                else if (ParsedCode[i2].Value == "override") {
                                    HasOverrideModifier = true;
                                }
                                else if (AccessModifiers.Contains(ParsedCode[i2].Value) && ParsedCode[i2].Value != "private") {
                                    HasAccessModifierWhichIsNotPrivate = true;
                                }
                            }
                            else {
                                break;
                            }
                        }
                        // Change return type to Task or Task<TResult> if async modifier present
                        if (HasAsyncModifier == true) {
                            string ReturnType = ParsedCode[i].Value;
                            ParsedCode[i].Value = "System.Threading.Tasks.Task";
                            if (ParsedCode[i].Value != "void") {
                                ParsedCode[i].Value += $"<{ReturnType}>";
                            }
                        }

                        // Insert virtual modifier if override and private modifiers are not present and task is not anonymous
                        if (HasOverrideModifier == false && HasAccessModifierWhichIsNotPrivate == true && IsAnonymousTask == false) {
                            ParsedCode.Insert(i, new Token(TokenType.Identifier, "virtual"));
                            i++;
                        }
                    }
                }

                // Try Catch Finally
                int TryDepth = 0;
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier) {
                        if (ParsedCode[i].Value == "Try") {
                            TryDepth++;
                            ParsedCode.Insert(i + 1, new Token(TokenType.StartTryOrCatchOrFinally));
                        }
                        else if (ParsedCode[i].Value == "Catch") {
                            if (TryDepth == 0) {
                                throw new Exception("Cannot have catch statement outside of try block.");
                            }
                            if (ParsedCode[i + 1].Type == TokenType.Identifier) {
                                ParsedCode.Insert(i + 3, new Token(TokenType.StartTryOrCatchOrFinally));
                            }
                        }
                        else if (ParsedCode[i].Value == "Finally") {
                            if (TryDepth == 0) {
                                throw new Exception("Cannot have finally statement outside of try block.");
                            }
                            ParsedCode.Insert(i + 1, new Token(TokenType.StartTryOrCatchOrFinally));
                        }
                    }
                }

                // Ensure readonly variables are not reassigned
                for (int i = 0; i < ParsedCode.Count; i++) {
                    if (ParsedCode[i].Type == TokenType.Identifier && ParsedCode[i].Value == "Readonly") {
                        if ((i + 3) < ParsedCode.Count && ParsedCode[i + 1].Type == TokenType.Identifier && ParsedCode[i + 2].Type == TokenType.Identifier && ParsedCode[i + 3].Type == TokenType.AssignmentOperator && ParsedCode[i + 3].Value == "=") {
                            string Identifier = ParsedCode[i + 2].Value;

                            // Loop through subsequent commands to find the identifier followed by an assignment operator
                            int Depth = 0;
                            for (int i2 = i + 3; i2 < ParsedCode.Count; i2++) {
                                if (ParsedCode[i2].Type == TokenType.Identifier) {
                                    // Check scope
                                    if (KeywordsStartingBlock.Contains(ParsedCode[i2].Value)) {
                                        Depth++;
                                    }
                                    else if (KeywordsEndingBlock.Contains(ParsedCode[i2].Value)) {
                                        if (Depth == 0) {
                                            // Out of scope
                                            break;
                                        }
                                        Depth--;
                                    }
                                    // Illegal reassignment
                                    else if (ParsedCode[i2].Value == Identifier) {
                                        if (i2 + 1 < ParsedCode.Count && ParsedCode[i2 + 1].Type == TokenType.AssignmentOperator) {
                                            throw new Exception("Readonly variables cannot be reassigned.");
                                        }
                                    }
                                }
                            }
                        }
                        else {
                            throw new Exception("Readonly variables must be set to a value and have a data type.");
                        }
                    }
                }

                return ParsedCode;
            }
            catch (Exception E) {
                throw new Exception($"There was an error parsing your code: '{E.Message}'");
            }
        }

        public static string Translate(List<Token> ParsedCode) {
            try {
                string TranslatedCode = "";

                TranslatedCode += "using PatchLanguage; using static PatchLanguage.Additions; using static PatchLanguage.Extensions; ";

                TokenType[] DoNotPutSemicolonAfter = new[] {TokenType.Semicolon, TokenType.Newline, TokenType.StartTask, TokenType.StartClassOrStruct, TokenType.StartIteration, TokenType.StartMatch, TokenType.StartCase, TokenType.StartTryOrCatchOrFinally, TokenType.AssignmentOperator, TokenType.ArithmeticOperator, TokenType.ConditionalOperator, TokenType.StartParameters, TokenType.Literal};

                for (int i = 0; i < ParsedCode.Count; i++) {
                    Token Token = ParsedCode[i];
                    switch (Token.Type) {
                        case TokenType.Newline:
                            if (i - 1 >= 0 && DoNotPutSemicolonAfter.Contains(ParsedCode[i - 1].Type) == false && (ParsedCode[i - 1].Type == TokenType.Identifier && ParsedCode[i - 1].Value == "End") == false) {
                                TranslatedCode += ";";
                            }
                            TranslatedCode += "\n";
                            break;
                        case TokenType.Semicolon:
                            TranslatedCode += ";";
                            break;
                        case TokenType.StartParameters:
                            TranslatedCode += "(";
                            break;
                        case TokenType.EndParameters:
                            TranslatedCode += ")";
                            break;
                        case TokenType.NextListItem:
                        case TokenType.NextParameter:
                            TranslatedCode += ", ";
                            break;
                        case TokenType.StartList:
                            TranslatedCode += "{";
                            break;
                        case TokenType.EndList:
                            TranslatedCode += "}";
                            break;
                        case TokenType.StartIndex:
                            TranslatedCode += "[";
                            break;
                        case TokenType.EndIndex:
                            TranslatedCode += "]";
                            break;
                        case TokenType.ArithmeticOperator:
                        case TokenType.AssignmentOperator:
                        case TokenType.ConditionalOperator:
                            TranslatedCode += " " + Token.Value + " ";
                            break;
                        case TokenType.Literal:
                            TranslatedCode += " " + Token.Value + " ";
                            break;
                        case TokenType.String:
                            int NumberOfNewlines = Token.Value.Count(x => x == '\n');
                            TranslatedCode += "\"" + Token.Value.Replace("\n", "\\n") + "\"" + new string('\n', NumberOfNewlines);
                            break;
                        case TokenType.Char:
                            TranslatedCode += "'" + Token.Value + "'";
                            break;
                        case TokenType.StartClassOrStruct:
                        case TokenType.StartTask:
                            TranslatedCode += " {";
                            break;
                        case TokenType.StartCase:
                            TranslatedCode += ":";
                            break;
                        case TokenType.StartIteration:
                        case TokenType.StartMatch:
                            TranslatedCode += ") {";
                            break;
                        case TokenType.StartTryOrCatchOrFinally:
                            if (ParsedCode[i - 1].Type != TokenType.Identifier || (ParsedCode[i - 1].Value != "Try" && ParsedCode[i - 1].Value != "Catch" && ParsedCode[i - 1].Value != "Finally")) {
                                TranslatedCode += ")";
                            }
                            TranslatedCode += " {";
                            break;
                        case TokenType.InheritsFrom:
                            TranslatedCode += " : ";
                            break;
                        case TokenType.BeginningOfDestructor:
                            TranslatedCode += "~";
                            break;
                        case TokenType.ElseInMatchCase:
                            TranslatedCode += " default";
                            break;
                        case TokenType.Identifier:
                            string Identifier = Token.Value.Replace('{', '<').Replace('}', '>');
                            switch (Identifier) {
                                case "If":
                                    Identifier = "if (";
                                    break;
                                case "Elseif":
                                    Identifier = "} else if (";
                                    break;
                                case "Else":
                                    Identifier = "} else ";
                                    break;
                                case "Then":
                                    Identifier = ") {";
                                    if (i - 1 >= 0 && ParsedCode[i - 1].Type == TokenType.Identifier && ParsedCode[i - 1].Value == "Else") {
                                        Identifier = Identifier[1..];
                                    }
                                    break;
                                case "End":
                                    Identifier = "}";
                                    if (i - 1 >= 0 && StatementSeparatorTokenTypes.Contains(ParsedCode[i - 1].Type) == false && (ParsedCode[i - 1].Type == TokenType.Identifier && ParsedCode[i - 1].Value == "End") == false) {
                                        Identifier = ";" + Identifier;
                                    }
                                    break;
                                case "Using":
                                    Identifier = "using";
                                    break;
                                case "Class":
                                    Identifier = "class";
                                    break;
                                case "Struct":
                                    Identifier = "struct";
                                    break;
                                case "Foreach":
                                    Identifier = "foreach (";
                                    break;
                                case "While":
                                    Identifier = "while (";
                                    break;
                                case "Match":
                                    Identifier = "switch (";
                                    break;
                                case "Case":
                                    Identifier = "case";
                                    break;
                                case "Const":
                                    Identifier = "const";
                                    break;
                                case "Readonly":
                                    Identifier = "";
                                    break;
                                case "Try":
                                    Identifier = "try";
                                    break;
                                case "Catch":
                                    Identifier = "} catch";
                                    if (ParsedCode[i + 1].Type == TokenType.Identifier) {
                                        Identifier += " (";
                                    }
                                    break;
                                case "Finally":
                                    Identifier = "} finally";
                                    break;
                            }
                            TranslatedCode += Identifier + " ";
                            break;
                    }
                }

                return TranslatedCode;
            }
            catch (Exception E) {
                throw new Exception($"There was an error translating your code: '{E.Message}'");
            }
        }

        public readonly static System.Reflection.Assembly[] DomainAssemblies = AppDomain.CurrentDomain.GetAssemblies(); // All of the available assemblies (e.g. System.Numerics.BigInteger)
        public static Script<object> Compile(string TranslatedCode) {
            try {
                Script<object> CompiledScript = CSharpScript.Create(TranslatedCode, ScriptOptions.Default.WithOptimizationLevel(OptimizationLevel.Release)
                    .WithReferences(DomainAssemblies).WithReferences(typeof(Microsoft.CSharp.RuntimeBinder.CSharpArgumentInfo).Assembly,
                    typeof(Additions).Assembly), typeof(RunGlobals));
                CompiledScript.Compile();
                return CompiledScript;
            }
            catch (Exception E) {
                throw new Exception($"There was an error compiling your code: '{E.Message}'");
            }
        }
        public static async Task<object> Run(Script<object> CompiledScript) {
            try {
                return (await CompiledScript.RunAsync(new RunGlobals())).ReturnValue;
            }
            catch (Exception E) {
                throw new Exception($"There was an error running your code: '{E.Message}'");
            }
        }
    }
}
