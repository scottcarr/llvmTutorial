#include <string>
#include <vector>
#include <map>
#include <cstdio>

// The lexer returns tokens [0-255] if it is an unknown character, otherwise one
// of these for known things.
enum Token { 
    tok_eof = -1,
    tok_def = -2,
    tok_extern = -3,
    tok_identifier = -4,
    tok_number = -5
};

static std::string IdentifierStr;
static double NumVal;
static int CurTok;

static int gettok() {
    static int LastChar = ' '; // inital value so we always enter loop
    while (isspace(LastChar)) {
        LastChar = getchar();
    }
    if (isalpha(LastChar))
    {
        // def and extern both start with characters (obviously)
        IdentifierStr = LastChar;
        // the identifier continues until we get a non-alphanumeric character
        while (isalnum(LastChar = getchar())) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") return tok_def;
        if (IdentifierStr == "extern") return tok_extern;

        // any string [a-zA-Z][a-zA-Z0-9]* other than 
        // "def" or "extern" is considered an identifier
        return tok_identifier; 
    }
    // numbers a start with [0-9] or '.'
    // this incorrectly makes 12.3.234.23434 a valid number
    if (isdigit(LastChar) || LastChar == '.')
    {
        std::string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar(); 
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }
    // eat and ignore comments
    if (LastChar == '#') {
        do {} while(LastChar != EOF && LastChar != '\n' && LastChar != '\r');
        if (LastChar != EOF)
        {
            return gettok();
        }
    }
    if (LastChar == EOF) {
        return tok_eof;
    }

    // otherwise just return char (error?)
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

static int getNextToken() {
    return CurTok = gettok();
}

class ExprAST
{
public:
    virtual ~ExprAST () {}
};

// numeric listerals like "1.0"
class NumberExprAST : public ExprAST
{
public:
    NumberExprAST (double val) : Val(val) {}

private:
    double Val;
};

class VariableExprAST : public ExprAST
{
public:
    VariableExprAST (const std::string &name) : Name(name) {}

private:
    std::string Name;
};

class BinaryExprAST : public ExprAST
{
public:
    BinaryExprAST (char op, ExprAST *lhs, ExprAST *rhs)
        : Op(op), LHS(lhs), RHS(rhs) {}

private:
    char Op;
    ExprAST *LHS, *RHS;
};

class CallExprAST : public ExprAST
{
public:
    CallExprAST (const std::string &callee, std::vector<ExprAST*> &args)
        : Callee(callee), Args(args) {}

private:
    std::string Callee;
    std::vector<ExprAST*> Args;
};

class PrototypeAST : public ExprAST
{
public:
    PrototypeAST (const std::string &name, const std::vector<std::string> &args)
        : Name(name), Args(args) {}

private:
    std::string Name;
    std::vector<std::string> Args;
};

class FunctionAST : public ExprAST
{
public:
    FunctionAST (PrototypeAST *proto, ExprAST *body)
        : Proto(proto), Body(body) {}

private:
    PrototypeAST *Proto;
    ExprAST *Body;
};

ExprAST *Error(const char *Str) { fprintf(stderr, "Error %s\n", Str); return 0; }
PrototypeAST *ErrorP(const char *Str) { Error(Str); return 0; }
FunctionAST *ErrorF(const char *Str) { Error(Str); return 0; }

// numberexpr ::= number
static ExprAST *ParseNumber() {
    ExprAST *Result = new NumberExprAST(NumVal);
    getNextToken(); // consume the number
    return Result;
}


static ExprAST *ParseIdentifierExpr();
static ExprAST *ParseParenExpr();

/// primary
///     ::= identifierexpr
///     ::= numberexpr
///     ::= parenexpr
static ExprAST *ParsePrimary() {
    switch(CurTok) {
        default: return Error("unknown token when expecting an expression");
        case tok_identifier: return ParseIdentifierExpr();
        case tok_number: return ParseNumber();
        case '(': return ParseParenExpr();
    }
}

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
    if (!isascii(CurTok)) { return -1; }
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec < 0) return -1;
    return TokPrec;
}

/// binoprhs
///     :: = ('+' primary)*
static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
    while (1) {
        int TokPrec = GetTokPrecedence();
        if (TokPrec < ExprPrec) return LHS;
        int BinOp = CurTok;
        getNextToken(); // eat binop
        ExprAST *RHS = ParsePrimary();
        if (!RHS) return 0;
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec+1, RHS);
            if (RHS == 0) return 0;
        }
        LHS = new BinaryExprAST(BinOp, LHS, RHS);
    }
}

static PrototypeAST *ParsePrototype() {
    if (CurTok != tok_identifier) return ErrorP("Expected function name in prototype");
    std::string FnName = IdentifierStr;
    getNextToken();
    if (CurTok != '(') return ErrorP("Expected '(' in prototype");
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') return ErrorP("Expected ')' in protoype");
    getNextToken(); // eat ')'
    return new PrototypeAST(FnName, ArgNames);
}

/// expression
///     ::= primpary binoprhs
static ExprAST *ParseExpression() {
    ExprAST* LHS = ParsePrimary();
    if (!LHS) return 0;
    return ParseBinOpRHS(0, LHS);
}

// parenexpr ::= '(' expression ')'
static ExprAST *ParseParenExpr() {
    getNextToken(); // eat (
    ExprAST *V = ParseExpression();
    if (!V) return 0;
    if (CurTok != ')') 
        return Error("expected ')'");
    getNextToken(); // eat )
    return V;
}

/// identifierexpr
///     ::= identifier
///     ::= identifier '(' expression ')'
static ExprAST *ParseIdentifierExpr() {
    std::string IdName = IdentifierStr;
    getNextToken(); // eat identifier
    if (CurTok != '(') // variable ref
        return new VariableExprAST(IdName);

    // Call
    getNextToken(); // eat (
    std::vector<ExprAST*> Args;
    if (CurTok != ')') {
        while(1) {
            ExprAST *Arg = ParseExpression();
            if (!Arg) return 0;
            Args.push_back(Arg);
            if (CurTok == ')') break;
            if (CurTok != ',')
                return Error("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }

    getNextToken(); // eat the )

    return new CallExprAST(IdName, Args);
}

/// definition ::= 'def' prototype expression
static FunctionAST *ParseDefintion() {
    getNextToken(); // eat def
    PrototypeAST *Proto = ParsePrototype();
    if (Proto == 0) return 0;

    if (ExprAST *E = ParseExpression()) {
        return new FunctionAST(Proto, E);
    }
    return 0;
}

static PrototypeAST *ParseExtern() {
    getNextToken(); // eat extern
    return ParsePrototype();
}

static FunctionAST *ParseTopLevelExpr() {
    if (ExprAST *E = ParseExpression()) {
        PrototypeAST  *Proto = new PrototypeAST("", std::vector<std::string>());
        return new FunctionAST(Proto, E);
    }
    return 0;
}


static void HandleDefintion() {
    if (ParseDefintion()) {
        fprintf(stdout, "Parsed a function defintion \n");
    } else {
        fprintf(stdout, "Error failed to parse defintion \n");
        getNextToken();
    }
}

static void HandleExtern() {
    if (ParseExtern()) {
        fprintf(stdout, "Parsed an extern\n");
    } else {
        fprintf(stdout, "Error failed to parse extern \n");
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    if (ParseTopLevelExpr()) {
        fprintf(stdout, "Parsed a top level expression\n");
    } else {
        fprintf(stdout, "Error failed to parse top level expression \n");
        getNextToken();
    }
}

static void MainLoop() {
    while (1) {
        fprintf(stderr, "ready> ");
        switch(CurTok) {
            case tok_eof: return;
            case ';': getNextToken(); break;
            case tok_def: HandleDefintion(); break;
            case tok_extern: HandleExtern(); break;
            default: HandleTopLevelExpression(); break;
        }
    }
}

int main() {
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;

    fprintf(stderr, "ready> ");
    getNextToken();
    MainLoop();
    return 0;
}
