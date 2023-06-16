/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    int yylex_destroy ();
    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    /* Used to generate code */
    /* As printf; the usage: CODEGEN("%d - %s\n", 100, "Hello world"); */
    /* We do not enforce the use of this macro */
    #define CODEGEN(...) \
        do { \
            for (int i = 0; i < g_indent_cnt; i++) { \
                fprintf(fout, "\t"); \
            } \
            fprintf(fout, __VA_ARGS__); \
        } while (0)

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static void insert_symbol(char*, char*, char*, int, bool);
    static char *lookup_symbol(char *, int);
    static int lookup_addr(char *, int);
    static void dump_symbol();
    static void build_func_para(char *);

    typedef struct table{
        struct table *upper_level;
        struct symbol *head;
        int scope;
        int symbol_number;
    } Table;

    Table *current_table = NULL;

    typedef struct symbol{
        int index;
        char* name;
        int mut;
        char* type;
        int addr;
        int lineno;
        char* func_sig;
        struct symbol *next;
    } Symbol;

    typedef struct label{
        int index;
        bool last;
        struct label *next;
    } Label;

    /* Global variables */
    bool g_has_error = false;
    FILE *fout = NULL;
    int g_indent_cnt = 0;

    bool HAS_ERROR = false;

    int level = -1;
    int addr = 0;
    int label_level = 0;
    int is_main = 0;
    int ptr_array = 0;

    char func_para[100] = "(";
    char return_type = 'z';
    char id_storage[10], id_temp[10];

    bool casting = false;
    bool need_return = false;
    bool while_loop = false;
    bool if_else = false;
    bool array_stmt = false;
    bool loop_stmt = false;

%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 *  - you can add new fields if needed.
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    /* ... */
}

/* Token without return */
%token LET MUT NEWLINE
%token INT FLOAT BOOL STR
%token TRUE FALSE
%token GEQ LEQ EQL NEQ LOR LAND
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN REM_ASSIGN
%token IF ELSE FOR WHILE LOOP
%token PRINT PRINTLN
%token FUNC RETURN BREAK
%token ID ARROW AS IN DOTDOT RSHIFT LSHIFT

/* Token with return, which need to sepcify type */
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> STRING_LIT

/* Nonterminal with return, which need to sepcify type */
%type <s_val> Type
%type <s_val> FuncName 
%type <s_val> Expression 
%type <s_val> LogicalORExpr LogicalANDExpr ComparisonExpr 
%type <s_val> AdditionExpr MultiplicationExpr 
%type <s_val> Cmp_OP Add_Sub Mul_Div_Rem Unary_OP Assign_OP Shift_OP 
%type <s_val> UnaryExpr Operand ArrayExpr Literal


/* Yacc will start at this nonterminal */
%start Program

/* Grammar section */
%%
Program
    : GlobalStatementList
;

GlobalStatementList 
    : GlobalStatementList GlobalStatement
    | GlobalStatement
;

GlobalStatement
    : FunctionDeclStmt
    | NEWLINE
;

FunctionDeclStmt
    : FuncName '(' 
    {
        create_symbol();
        need_return = true;
    }
    Paras ')' ARROW Type 
    {
        insert_symbol("func", $<s_val>2, func_para, 0, false);
        // printf("> Insert `%s` (addr: -1) to scope level %d\n", $<s_val>2, 0);
        if(is_main == 0){
            CODEGEN("(V)V\n");
            CODEGEN(".limit stack 20\n");
            CODEGEN(".limit locals 20\n");
        } else {
            // is_main = 0;
        }
        
        build_func_para($<s_val>6);
        strcpy(func_para, "(");
        if(strcmp($<s_val>7, "bool") == 0){
            return_type = 'b';
        }
        // return_type = ($<s_val>6[0]);
    }
    FuncContent
    {
        need_return = false;
        return_type = 'v';
    }
    | FuncName '(' ')' 
    {   
        insert_symbol("func", $<s_val>2, "()V", 0, false);
        // printf("> Insert `%s` (addr: -1) to scope level %d\n", $<s_val>2, 0);
        create_symbol();
        if(is_main == 0){
            CODEGEN("()V\n");
            CODEGEN(".limit stack 20\n");
            CODEGEN(".limit locals 20\n");
        } else {
            // is_main = 0;
        }
    }
    FuncContent    
;
FuncName
    : FUNC ID 
    {
        printf("func: %s\n", $<s_val>2);
        if(strcmp($<s_val>2, "main") == 0){
            CODEGEN(".method public static main([Ljava/lang/String;)V\n");
            CODEGEN(".limit stack 100\n");
            CODEGEN(".limit locals 100\n");
            is_main = 1;
        } else {
            CODEGEN(".method public static %s", $<s_val>2);
            is_main = 0;
        }
        $$ = $<s_val>2;
    }
;

FuncContent
    : '{' NEWLINE StatementList '}' 
    { 
        if(need_return){
            // printf("%creturn\n", return_type);
            if(return_type == 'b'){
                CODEGEN("iconst_1\n");
                CODEGEN("L_exit:\n");
                CODEGEN("ireturn\n");
            } else {
                CODEGEN("L_exit:\n");
                CODEGEN("%creturn\n", return_type);
            }
            // return_type = 'z';
            need_return = false;
        } else {
            CODEGEN("return\n");
            is_main = 0;
        }
        dump_symbol();
        CODEGEN(".end method");
    }
;

Paras
    : Paras  ',' ID ':' Type
    {
        insert_symbol($<s_val>5, $<s_val>3, "-", 1, false);
        printf("> Insert `%s` (addr: %d) to scope level %d\n", $<s_val>3, addr-1, level);
        build_func_para($<s_val>5);
    }

    | ID ':' Type
    {
        insert_symbol($<s_val>3, $<s_val>1, "-", 1, false);
        printf("> Insert `%s` (addr: %d) to scope level %d\n", $<s_val>1, addr-1, level);
        build_func_para($<s_val>3);
    }
;

StatementList
    : Statement
    | Statement StatementList
;
Statement
    : Expression
    | Declaration ';' NEWLINE
    | Assignment ';' NEWLINE
    | Print ';' NEWLINE
    | If_Else 
    | Loop
    | While_Loop NEWLINE
    | For_Loop NEWLINE
    | Call_Func NEWLINE;
    | Content NEWLINE 
    | NEWLINE
;

Expression
    : LogicalORExpr {$$ = $1;}
;

Declaration
    : LET ID ':' Type 
    { 
        insert_symbol($<s_val>4, $<s_val>2, "-", 2, false ); 
        if(strcmp($<s_val>4, "i32") == 0){
            CODEGEN("ldc 0\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "f32") == 0){
            CODEGEN("ldc 0.000000\n");
            CODEGEN("fstore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "str") == 0){
            CODEGEN("ldc \"\\n\"\n");
            CODEGEN("astore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "true") == 0){
            CODEGEN("ldc iconst_1\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "false") == 0){
            CODEGEN("ldc iconst_0\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
    }
    | LET MUT ID ':' Type 
    { 
        insert_symbol($<s_val>5, $<s_val>3, "-", 2, true ); 
        if(strcmp($<s_val>5, "i32") == 0){
            CODEGEN("ldc 0\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "f32") == 0){
            CODEGEN("ldc 0.000000\n");
            CODEGEN("fstore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "str") == 0){
            CODEGEN("ldc \"\\n\"\n");
            CODEGEN("astore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "true") == 0){
            CODEGEN("ldc iconst_1\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "false") == 0){
            CODEGEN("ldc iconst_0\n");
            CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
        }
    }
    | LET ID ':' Type '=' StatementList 
    { 
        insert_symbol($<s_val>4, $<s_val>2, "-", 2, false ); 
        if(strcmp($<s_val>4, "i32") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "f32") == 0){
            CODEGEN("fstore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "str") == 0){
            CODEGEN("astore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "bool") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
    }
    | LET MUT ID ':' Type '=' Expression 
    { 
        insert_symbol($<s_val>5, $<s_val>3, "-", 2, true ); 
        if(strcmp($<s_val>5, "i32") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "f32") == 0){
            CODEGEN("fstore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "str") == 0){
            CODEGEN("astore %d\n", lookup_addr($<s_val>3, 2));
        }
        else if(strcmp($<s_val>5, "bool") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
        }
    }
    | LET ID ':' Declare_Array 
    {
        array_stmt = true;
    }
    '=' Expression 
    { 
        insert_symbol("array", $<s_val>2, "-", 2, false ); 
        CODEGEN("astore_1\n");
    }
    | LET MUT ID '=' Expression 
    {
        insert_symbol("i32", $<s_val>3, "-", 2, true ); 
        CODEGEN("istore %d\n", lookup_addr($<s_val>3, 2));
    }
    | LET ID '=' Literal 
    { 
        insert_symbol($<s_val>4, $<s_val>2, "-", 2, false); 
        if(strcmp($<s_val>4, "i32") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "f32") == 0){
            CODEGEN("fstore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "str") == 0){
            CODEGEN("astore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "true") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "false") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
    }
    | LET ID ':' Type '=' Loop
    {
        insert_symbol($<s_val>4, $<s_val>2, "-", 2, false); 
        if(strcmp($<s_val>4, "i32") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "f32") == 0){
            CODEGEN("fstore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "str") == 0){
            CODEGEN("astore %d\n", lookup_addr($<s_val>2, 2));
        }
        else if(strcmp($<s_val>4, "bool") == 0){
            CODEGEN("istore %d\n", lookup_addr($<s_val>2, 2));
        }
    }
;

Assignment
    : Expression
    {
        strcpy(id_storage, id_temp);
    } 
    Assign_OP Expression 
    { 
        // if(strcmp(lookup_symbol($<s_val>1, 3), "undefined")!=0)
        //     printf("%s\n", $<s_val>2); 
        if(strcmp($<s_val>3, "ADD_ASSIGN") == 0){
            CODEGEN("%cadd\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "SUB_ASSIGN") == 0){
            CODEGEN("%csub\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "MUL_ASSIGN") == 0){
            CODEGEN("%cmul\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "DIV_ASSIGN") == 0){
            CODEGEN("%cdiv\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "REM_ASSIGN") == 0){
            CODEGEN("%crem\n", $<s_val>1[0]);
        }

        if(strcmp($<s_val>1, "str") == 0){
            CODEGEN("astore %d\n", lookup_addr(id_storage, 2));
        }
        else if(strcmp($<s_val>1, "bool") == 0){
            CODEGEN("istore %d\n", lookup_addr(id_storage, 2));
        }
        else {
            CODEGEN("%cstore %d\n", $<s_val>1[0], lookup_addr(id_storage, 2));
        }
    }
;

Print
    : PRINT '(' NEWLINE Expression NEWLINE ')' 
    {
        printf("PRINT %s\n", $<s_val>4);
        if(strcmp($<s_val>4, "bool") == 0){
            CODEGEN("ifne L_%d\n", label_level++);
            CODEGEN("ldc \"false\"\n");
            CODEGEN("goto L_%d\n", label_level++);
            CODEGEN("L_%d:\n", label_level-2);
            CODEGEN("ldc \"true\"\n");
            CODEGEN("L_%d:\n", label_level-1);
        }
        CODEGEN("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        CODEGEN("swap\n");
        CODEGEN("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
    | PRINT '('  Expression ')' 
    { 
        printf("PRINT %s\n", $<s_val>3);
        if(strcmp($<s_val>3, "bool")==0){
            CODEGEN("ifne L_%d\n", label_level++);
            CODEGEN("ldc \"false\"\n");
            CODEGEN("goto L_%d\n", label_level++);
            CODEGEN("L_%d:\n", label_level-2);
            CODEGEN("ldc \"true\"\n");
            CODEGEN("L_%d:\n", label_level-1);
        }
        CODEGEN("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        CODEGEN("swap\n");
        CODEGEN("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
    | PRINTLN '(' Expression ')'  
    {
        printf("PRINTLN %s\n", $<s_val>3);
        if(strcmp($<s_val>3, "bool")==0){
            CODEGEN("ifne L_%d\n", label_level++);
            CODEGEN("ldc \"false\"\n");
            CODEGEN("goto L_%d\n", label_level++);
            CODEGEN("L_%d:\n", label_level-2);
            CODEGEN("ldc \"true\"\n");
            CODEGEN("L_%d:\n", label_level-1);
        }
        CODEGEN("getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        CODEGEN("swap\n");
        if(strcmp($<s_val>3, "bool") == 0 || strcmp($<s_val>3, "str") == 0){
            CODEGEN("invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        }
        else if(array_stmt){
            CODEGEN("invokevirtual java/io/PrintStream/println(I)V\n");
            array_stmt = false;
        }
        else {
            CODEGEN("invokevirtual java/io/PrintStream/println(%c)V\n", toupper($<s_val>3[0]));
        }
    }
;

If_Else
    : IF 
    {
        if_else = true;
    }
    Expression Content 
    {
        if(loop_stmt){
            CODEGEN("goto L_loop_end\n");
        }
        CODEGEN("L_if_%d: \n", label_level - 1);
    }
    NEWLINE
    | IF 
    {
        if_else = true;
    }
    Expression Content NEWLINE ELSE 
    {
        CODEGEN("L_if_%d: \n", label_level - 1);
    }
    Content
    | IF 
    {
        if_else = true;
    }
    Expression Content ELSE 
    {
        CODEGEN("L_if_%d: \n", label_level - 1);
    }
    Content
;

Loop
    : LOOP 
    {
        CODEGEN("L_loop:\n");
        loop_stmt = true;
    }
    Content
;

While_Loop
    : WHILE 
    {
        while_loop = true;
        CODEGEN("L_while_%d: \n", label_level++);
    }
    Expression 
    {
        CODEGEN("iflt L_while_%d\n", label_level++);
        CODEGEN("goto L_while_%d\n", label_level++);
        CODEGEN("L_while_%d: \n", label_level-2);
    }
    Content
    {
        CODEGEN("goto L_while_%d\n", label_level-3);
        CODEGEN("L_while_%d: \n", label_level-1);
    }
;

For_Loop
    : FOR ID IN ID 
    {
        lookup_symbol($<s_val>4, 2);
    }
    '{' NEWLINE
    { 
        create_symbol(); 
    }
    {
        insert_symbol("i32", $<s_val>2, "-", 3, false);
    } 
    StatementList '}'
    {
        dump_symbol();
    }
;

Call_Func
    : ID '(' ')' ';' 
    { 
        lookup_symbol($<s_val>1, 0); 
    }
;

Content
    : '{' NEWLINE 
    { 
        create_symbol(); 
    }
    RETURN Literal ';' NEWLINE '}'
    {
        // printf("breturn\n");
        if(strcmp($<s_val>4, "true") == 0){
            return_type = 'b';
            CODEGEN("iconst_1\n");
            CODEGEN("goto L_exit\n");
        } else if(strcmp($<s_val>4, "false") == 0){
            return_type = 'b';
            CODEGEN("iconst_0\n");
            CODEGEN("goto L_exit\n");
        }
        dump_symbol();
        
    }

    | '{' NEWLINE
    { 
        create_symbol(); 
    } 
    StatementList '}' 
    { 
        dump_symbol(); 
        if(if_else){
            if_else = false;
        }
        if(loop_stmt){
            CODEGEN("goto L_loop\n");
        }
    }

    | '{' NEWLINE
    { 
        create_symbol(); 
    } 
    BREAK Literal ';' NEWLINE '}' 
    { 
        dump_symbol();
    }   
;

Declare_Array
    : '[' Declare_Array ']'
    | Type ';' Literal
    {
        if(strcmp($<s_val>1, "i32") == 0){
            CODEGEN("newarray int\n");
        }
    }
;

LogicalORExpr
    : LogicalANDExpr LOR LogicalANDExpr
    {
        $$ = "bool";
        printf("LOR\n");
        CODEGEN("ior\n");
    }
    | LogicalANDExpr { $$ = $1; }
;
LogicalANDExpr
    : ComparisonExpr LAND ComparisonExpr
    {
        $$ = "bool"; 
        printf("LAND\n");
        CODEGEN("iand\n");
    }
    | ComparisonExpr { $$ = $1; }
;
ComparisonExpr
    : AdditionExpr Cmp_OP AdditionExpr
    {
        if(strcmp($<s_val>1, $<s_val>3) != 0){
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno+1, $<s_val>2, $<s_val>1, $<s_val>3);
        } else {
            if($<s_val>1[0] == 'i'){
                CODEGEN("isub\n");
            }
            else if($<s_val>1[0] == 'f'){
                CODEGEN("fcmpl\n");
            }
            if(strcmp($<s_val>2, "GTR") == 0){
                CODEGEN("ifgt L%d\n", label_level++);
                CODEGEN("iconst_0\n");
                CODEGEN("goto L%d\n", label_level++);
                CODEGEN("L%d:\n", label_level-2);
                CODEGEN("iconst_1\n");
                CODEGEN("L%d:\n", label_level-1);
            }
            else if(strcmp($<s_val>2, "LES") == 0){
                CODEGEN("iflt L%d\n", label_level++);
                CODEGEN("iconst_0\n");
                CODEGEN("goto L%d\n", label_level++);
                CODEGEN("L%d:\n", label_level-2);
                CODEGEN("iconst_1\n");
                CODEGEN("L%d:\n", label_level-1);
            }
            else if(strcmp($<s_val>2, "EQL") == 0){
                CODEGEN("ifeq L_if_%d\n", label_level++);
                CODEGEN("goto L_if_%d\n", label_level++);
                CODEGEN("L_if_%d:\n", label_level-2);
                if(!if_else){
                    CODEGEN("L_if_%d:\n", label_level-1);    
                }
                else
                    if_else = false;
                
            }
        }
        $$ = "bool";
        printf("%s\n", $<s_val>2);

    }
    | AdditionExpr { $$ = $1; }
;
AdditionExpr
    : MultiplicationExpr Add_Sub MultiplicationExpr
    {
        
        printf("%s\n", $<s_val>2);
        $$ = $1; 
        CODEGEN("%c%s\n", tolower($<s_val>1[0]), $<s_val>2);
    }
    | AdditionExpr Add_Sub MultiplicationExpr
    {
        printf("%s\n", $<s_val>2);
        $$ = $1; 
        CODEGEN("%c%s\n", tolower($<s_val>1[0]), $<s_val>2);
        
    }
    | MultiplicationExpr { $$ = $1; }
;

MultiplicationExpr
    : UnaryExpr Mul_Div_Rem UnaryExpr
    {
        $$ = $1;
        printf("%s\n", $<s_val>2);
        CODEGEN("%c%s\n", tolower($<s_val>1[0]), $<s_val>2);

    }
    | UnaryExpr Shift_OP UnaryExpr
    {
        if(strcmp($<s_val>1, $<s_val>3) != 0){
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno+1, $<s_val>2, $<s_val>1, $<s_val>3);
        }
        $$ = $1;
        printf("%s\n", $<s_val>2);
        
    }
    | UnaryExpr { $$ = $1; }
;
UnaryExpr
    : Unary_OP UnaryExpr 
    { 
        $$ = $2; 
        printf("%s\n", $<s_val>1); 
        if(strcmp($<s_val>1, "NOT") == 0){
            CODEGEN("iconst_0\n");
            if(strcmp($<s_val>2, "true") == 0){
                CODEGEN("iconst_1\n");
            } else if(strcmp($<s_val>2, "false") == 0){
                CODEGEN("iconst_0\n");
            }
            CODEGEN("iand\n");
            $$ = "bool";
        } else if(strcmp($<s_val>1, "neg") == 0){
            CODEGEN("%cneg\n", $<s_val>2[0]);
        }
    }
    | ArrayExpr {$$ = $1;}
    | Operand 
    { 
        $$ = $1; 
        if(strcmp($<s_val>1, "true") == 0){
            CODEGEN("iconst_1\n");
            $$ = "bool";
        } else if(strcmp($<s_val>1, "false") == 0){
            CODEGEN("iconst_0\n");
            $$ = "bool";
        }
    }
;
ArrayExpr
    : Literal ',' ArrayExpr
    | '[' Literal ',' ArrayExpr
    | Literal ']'
    | '&' ID 
    {
        lookup_symbol($<s_val>2, 2);
    }
    '[' DotExpr ']'
;
DotExpr
    : DOTDOT 
    {
        printf("DOTDOT\n");
    }
    Literal 
    | Literal DOTDOT
    {
        printf("DOTDOT\n");
    }
    | Literal DOTDOT
    {
        printf("DOTDOT\n");
    }
    Literal
;

Operand
    : Literal 
    { 
        $$ = $1; 
    }
    | Literal AS 
    { 
        casting = true; 
    } 
    Type
    {
        $$ = $<s_val>4;
    }
    | ID 
    { 
        $$ = lookup_symbol($<s_val>1, 2);
    }
    | '(' Expression ')' 
    { 
        $$ = $2; 
    }
    | ID '[' INT_LIT ']' 
    { 
        $$ = lookup_symbol($<s_val>1, 2); 
        // printf("INT_LIT %d\n", $<i_val>3);
        CODEGEN("aload_1\n");
        CODEGEN("iconst_%d", $<i_val>3);
        CODEGEN("iaload\n");
        array_stmt = true;
    }
    | ID AS 
    {
        casting = true;
        lookup_symbol($<s_val>1, 2) ;
    }
    Type 
    {
        $$ = $<s_val>4;
    }
    | ID '(' ID ',' ID ')'
    {
        lookup_symbol($<s_val>3, 1);
        lookup_symbol($<s_val>5, 1);
        need_return = true;
        return_type = 'b';
        lookup_symbol($<s_val>1, 0);
        CODEGEN("ifeq L_if_%d\n", label_level++);
        CODEGEN("goto L_if_%d\n", label_level++);
        CODEGEN("L_if_%d: \n", label_level-2);
    }
;

Literal
    : INT_LIT 
    { 
        $$ = "i32"; 
        // printf("INT_LIT %d\n", $<i_val>1);
        if(while_loop){
            while_loop = false;
            CODEGEN("ldc %d\n", $<i_val>1);
        }
        else if(array_stmt){
            CODEGEN("dup\n");
            CODEGEN("iconst_%d\n", ptr_array++);
            CODEGEN("bipush %d\n", $<i_val>1);
            CODEGEN("iastore\n");
        }
        else {
            CODEGEN("ldc %d\n", $<i_val>1);
        }
        
    }
    | FLOAT_LIT 
    { 
        $$ = "f32"; 
        // printf("FLOAT_LIT %f\n", $<f_val>1);
        CODEGEN("ldc %f\n", $<f_val>1);
    }
    | '"' STRING_LIT '"' 
    { 
        $$ = "str"; 
        // printf("STRING_LIT \"%s\"\n", $<s_val>2);
        CODEGEN("ldc \"%s\"\n", $<s_val>2);
    }
    | '"''"' 
    { 
        $$ = "str"; 
        // printf("STRING_LIT \"\"\n");
        CODEGEN("ldc \"\"\n");
    }
    | TRUE 
    { 
        $$ = "true"; 
        // printf("bool TRUE\n");
    }
    | FALSE 
    { 
        $$ = "false"; 
        // printf("bool FALSE\n");
    }
;

Assign_OP
    : '=' {$$ = "ASSIGN";}
    | ADD_ASSIGN {$$ = "ADD_ASSIGN";}
    | SUB_ASSIGN {$$ = "SUB_ASSIGN";}
    | MUL_ASSIGN {$$ = "MUL_ASSIGN";}
    | DIV_ASSIGN {$$ = "DIV_ASSIGN";}
    | REM_ASSIGN {$$ = "REM_ASSIGN";}
;

Cmp_OP 
    : EQL { $$ = "EQL"; }
    | NEQ { $$ = "NEQ"; }
    | '<' { $$ = "LSS"; }
    | LEQ { $$ = "LEQ"; }
    | '>' { $$ = "GTR"; }
    | GEQ { $$ = "GEQ"; }
;

Add_Sub 
    : '+' { $$ = "add"; }
    | '-' { $$ = "sub"; }
;

Mul_Div_Rem 
    : '*' { $$ = "mul"; }
    | '/' { $$ = "div"; }
    | '%' { $$ = "rem"; }
;

Shift_OP
    : LSHIFT { $$ = "ishl"; }
    | RSHIFT { $$ = "iushr"; }
;

Unary_OP 
    : '+' { $$ = "pos"; }
    | '-' { $$ = "neg"; }
    | '!' { $$ = "not"; }
;

Type 
    : INT		
    { 
        $$ = "i32";
        if(casting){
            casting = false;
            printf("f2i\n");
            CODEGEN("f2i\n");
        } 
    }
	| FLOAT		
    { 
        $$ = "f32";
        if(casting){
            casting = false;
            printf("i2f\n");
            CODEGEN("i2f\n");
        } 
    }
	| '&' STR	    
    { 
        $$ = "str"; 
    }
	| BOOL		
    { 
        $$ = "bool"; 
    }
;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }
    if (!yyin) {
        printf("file `%s` doesn't exists or cannot be opened\n", argv[1]);
        exit(1);
    }

    /* Codegen output init */
    char *bytecode_filename = "hw3.j";
    fout = fopen(bytecode_filename, "w");
    CODEGEN(".source hw3.j\n");
    CODEGEN(".class public Main\n");
    CODEGEN(".super java/lang/Object\n");

    /* Symbol table init */
    // Add your code

    create_symbol();
    yylineno = 0;
    yyparse();
    dump_symbol();
    /* Symbol table dump */
    // Add your code

	printf("Total lines: %d\n", yylineno);
    fclose(fout);
    fclose(yyin);

    if (g_has_error) {
        remove(bytecode_filename);
    }
    yylex_destroy();
    return 0;
}

static void create_symbol() {
    level++;
    Table *new_table = malloc(sizeof(Symbol));
    // Table: Link upper_level, Symbol link head, Scope, Symbol_number
    new_table->upper_level = current_table;
    new_table->scope = level;
    new_table->symbol_number = 0;
    new_table->head = NULL;
    current_table = new_table;
    /* printf("> Create symbol table (scope level %d)\n", level); */
}

static void insert_symbol(char* type, char* name, char* func_sig, int mark, bool has_mut) {
    // if mark = 0, function; else if mark = 1, parameter, else if mark == 2, id, else(foreach), mark == 3
    Symbol *tail = NULL;
    Table *first = current_table;
    bool empty = false;
    if(first->head != NULL){
        tail = first->head;
        while(tail->next != NULL)
            tail = tail->next;
    }
    else { // empty
        empty = true;
    }
    
    Symbol *next_symbol = malloc(sizeof(Symbol));
    
    // Symbol: Index, Name, Mut, Type, Addr, Lineno, Func_sig, Link next
    if(!empty){ // Index
        next_symbol->index = (tail->index) + 1;
        tail->next = next_symbol;
    }
    else{
        next_symbol->index = 0;
        first->head = next_symbol;
    }
    
    next_symbol->name = strdup(name); // Name

    if(mark == 0) // Mut
        next_symbol->mut = -1;
    else if(has_mut)
        next_symbol->mut = 1;
    else 
        next_symbol->mut = 0;

    next_symbol->type = strdup(type); // Type

    // Addr
    if(mark == 0){ // Function
        next_symbol->addr = -1;
    } else { // Parameter, Id, Foreach
        next_symbol->addr = addr;
        addr++;
    }

    // Lineno
    if(mark == 0 || mark == 1 || mark == 2){ // Function, Parameter, Id
        next_symbol->lineno = yylineno + 1;
    } else { // Foreach
        next_symbol->lineno = yylineno;
    }

    // Func_sig
    next_symbol->func_sig = strdup(func_sig);

    // Next
    next_symbol->next = NULL;
    
    // Print
    /* if(mark == 2 || mark == 3)// Id, Foreach
        printf("> Insert `%s` (addr: %d) to scope level %d\n", name, next_symbol -> addr, level); */
}

static char *lookup_symbol(char *name, int mark) { // if mark = 0, function; else if mark = 1, parameter, else(id) is 2
    Table *target_table = current_table;
    Symbol *target_symbol = NULL;
    while(target_table != NULL){
        target_symbol = target_table->head;
        while(target_symbol != NULL){
            if(strcmp(target_symbol->name, name) == 0){ // Find the name
                if(mark == 0) { // function
                    if(return_type == 'v'){
                        CODEGEN("invokestatic Main/%s%s\n", target_symbol->name, target_symbol->func_sig);
                    }
                    else if(return_type == 'b'){
                        CODEGEN("invokestatic Main/%s%s%c\n", target_symbol->name, target_symbol->func_sig, toupper(return_type));
                    }
                    if(need_return){
                        strcat(func_para, ")");
                        printf("call: %s(II)B\n", target_symbol->name);
                        need_return = false;
                        /* target_symbol->func_sig = "(II)B"; */
                        if(return_type == 'b'){
                            target_symbol->func_sig = strcat(func_para, "B");
                        }
                        return target_symbol->func_sig ;
                    }
                    else{
                        printf("call: %s%s\n", target_symbol->name, target_symbol->func_sig);
                        /* CODEGEN("invokestatic Main/%s%s\n", target_symbol->name, target_symbol->func_sig); */
                        return target_symbol->func_sig;
                    }
                } else if(mark != 3){// not function
                    printf("IDENT (name=%s, address=%d)\n", target_symbol->name, target_symbol->addr);
                    if(strcmp(target_symbol->type, "i32") == 0 || strcmp(target_symbol->type, "bool") == 0){
                        CODEGEN("iload %d\n", target_symbol->addr);
                    } else if(strcmp(target_symbol->type, "f32") == 0){
                        CODEGEN("fload %d\n", target_symbol->addr);
                    } else if(strcmp(target_symbol->type, "str") == 0){
                        CODEGEN("aload %d\n", target_symbol->addr);
                    }
                    strcpy(id_temp, target_symbol->name);
                    return target_symbol->type;
                } else {
                    return target_symbol->type;
                }
            } else { // name is not same
                target_symbol = target_symbol->next;
            }
        }
        target_table = target_table->upper_level;
    }
    /* if it's not return until this step, it is represent the symbol is an error */
    printf("error:%d: undefined: %s\n", yylineno+1, name);
    g_has_error = true;
    return "undefined";
}

static int lookup_addr(char* name, int mark){
    Table *target_table = current_table;
    Symbol *target_symbol = NULL;
    while(target_table != NULL){
        target_symbol = target_table->head;
        while(target_symbol != NULL){
            if(strcmp(target_symbol->name, name) == 0){ // Find the name
                if(mark == 0) { // function
                    printf("call: %s%s\n", target_symbol->name, target_symbol->func_sig);
                    return target_symbol->func_sig;
                } else if(mark != 3){// not function
                    return target_symbol->addr;
                } else {
                    return target_symbol->addr;
                }
            } else { // name is not same
                target_symbol = target_symbol->next;
            }
        }
        target_table = target_table->upper_level;
    }
    /* if it's not return until this step, it is represent the symbol is an error */
    printf("error:%d: undefined: %s\n", yylineno+1, name);
    return "undefined";
}

static void dump_symbol() {
    /* printf("\n> Dump symbol table (scope level: %d)\n", current_table -> scope);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n",
        "Index", "Name", "Mut","Type", "Addr", "Lineno", "Func_sig"); */
    
    Symbol *tmp = current_table->head;
    current_table = current_table->upper_level;
    level--;
    while(tmp!= NULL){
        /* printf("%-10d%-10s%-10d%-10s%-10d%-10d%-10s\n",
            tmp-> index, tmp-> name, tmp-> mut, tmp-> type, tmp-> addr, tmp-> lineno, tmp-> func_sig); */
        tmp = tmp->next;
    }
}

static void build_func_para(char *para) {
    char temp = para[0];
    temp = toupper(temp);
    strcat(func_para, &temp);
}
