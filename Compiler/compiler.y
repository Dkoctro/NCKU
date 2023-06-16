/* Please feel free to modify any content */

/* Definition section */
%{
    #include "compiler_common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1; 
    # define Fu 0 // function
    # define P 1 // parameter
    # define I 2 // id
    # define Fo 3 // foreach

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

    /* Symbol table function - you can add new functions if needed. */
    /* parameters and return type can be changed */
    static void create_symbol();
    static void insert_symbol(char*, char*, char*, int, bool);
    static void dump_symbol();
    static void build_func_para(char *);

    struct table{
        int scope;
        struct table *prev;
        struct symbol *head;
    };

    struct symbol{
        int index;
        char* name;
        int mut;
        char* type;
        int addr;
        int lineno;
        char* func_sig;
        struct symbol *next;
    };
    struct table *current_table = NULL;


    
    /* Global variables */
    bool HAS_ERROR = false;
    int global_scope = -1;
    int addr = 0;
    bool casting = false;
    char func_para[100] = "(";
    bool has_return = false;
    char return_type = 'v';



    /* hw3 */
    static int lookup_symbol_address(char*, int);
    static char *lookup_symbol(char*, int);
    // static void code_generation();
    FILE *fp = NULL;
    int label_number = 0;
    char id_storage[10], id_temp[10];
    int is_main = 0;
    bool is_while = false;
    int cmp_op = 0;
    int function_label = 0;
    int load_num = 0;
    char* parameter_stack[10];
    int ptr_parameter = 0;
    // char para_print[100];
    bool is_if = false;
    bool is_array = false;
    int ptr_array = 0;
    int number_arr = 0;
    bool g_has_error = false;
    bool loop = false;
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
%type <s_val> add_op mul_op assign_op unary_op cmp_op shift_op Type Literal
%type <s_val> FuncOpen ExpressionStmt LogicalORExpr LogicalANDExpr ComparisonExpr AdditionExpr MultiplicationExpr UnaryExpr Operand 

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
    : FuncOpen '(' 
    {
        
        printf("> Insert `%s` (addr: -1) to scope level %d\n", $<s_val>2, 0);
        create_symbol();
        has_return = true;
    }
    ParameterList ')' ARROW Type
    {
        
        
        strcat(func_para, ")");
        if(strcmp($<s_val>7, "bool")==0){
            return_type = 'b';
        }
        insert_symbol("func", $<s_val>2, func_para, Fu, false);
        if(is_main == 0){
            fprintf(fp, "%s%c\n", func_para, toupper(return_type));
            fprintf(fp, ".limit stack 20\n");
            fprintf(fp, ".limit locals 20\n");
        }
    }
    FuncBlock
    {
        has_return = false;
        return_type = 'v';
    }

    | FuncOpen '(' ')'  
    {   
        insert_symbol("func", $<s_val>2, "()V", Fu, false);
        printf("> Insert `%s` (addr: -1) to scope level %d\n", $<s_val>2, 0);
        create_symbol();

        if(is_main == 0){
            fprintf(fp, "()V\n");
            fprintf(fp, ".limit stack 20\n");
            fprintf(fp, ".limit locals 20\n");
        }
        else{
            // is_main =0;
        }
    }
    FuncBlock
;
FuncOpen
    : FUNC ID 
    {
        printf("func: %s\n", $<s_val>2);
        $$ = $<s_val>2;
        if(strcmp($<s_val>2, "main") == 0){
            fprintf(fp, ".method public static main([Ljava/lang/String;)V\n");
            fprintf(fp, ".limit stack 100\n");
            fprintf(fp, ".limit locals 100\n");
            is_main = 1;
        }
        else{
            fprintf(fp, ".method public static %s", $<s_val>2);
            is_main = 0;
        }
    }
;

ParameterList
    :  ParameterList  ',' ID ':' Type
    {
        insert_symbol($<s_val>5, $<s_val>3, "-", P, false);
        // parameter_stack[ptr_parameter++] = $<s_val>3;
        printf("> Insert `%s` (addr: %d) to scope level %d\n", $<s_val>3, addr-1, global_scope);
        build_func_para($<s_val>5);
    }
    | ID ':' Type
    {
        insert_symbol($<s_val>3, $<s_val>1, "-", P, false);
        // parameter_stack[ptr_parameter++] = $<s_val>1;
        printf("> Insert `%s` (addr: %d) to scope level %d\n", $<s_val>1, addr-1, global_scope);
        build_func_para($<s_val>3);
        
    }
;
FuncBlock
    : '{' NEWLINE StatementList '}' 
    { 
        
        if(has_return){
            if (return_type == 'b'){
                fprintf(fp, "iconst_1\n");
                fprintf(fp, "L_exit:\n\n");
                fprintf(fp, "ireturn\n");
            } else {
                fprintf(fp, "L_exit:\n\n");
                fprintf(fp, "%creturn\n", return_type);
            }
            has_return = false;
        }
        else if(is_main == 0){
            fprintf(fp, "return\n");
        }
        else {
            fprintf(fp, "return\n");
            is_main = 0;
        }
            
        dump_symbol(); 
        
        fprintf(fp, ".end method\n");
        
    }
;
StatementList
    : Statement
    | Statement StatementList // statement越上面先做
;
Statement
    : Block NEWLINE // multi-statement
    | DeclarationStmt ';' NEWLINE
    | ExpressionStmt
    | AssignmentStmt ';' NEWLINE
    | IFStmt 
    | PrintStmt ';' NEWLINE
    | WhileStmt NEWLINE
    | ForStmt NEWLINE
    | CallFunction NEWLINE;
    | NEWLINE
;
CallFunction
    : ID '(' ')' ';' 
    { 
        // addr = 0;
        lookup_symbol($<s_val>1, Fu); // function

    }
;
Block
    : StartBlock RETURN Literal ';' NEWLINE '}'
    {
        if(strcmp($<s_val>3, "true") == 0 || strcmp($<s_val>3, "false") == 0){
            return_type = 'b';
            printf("%creturn\n" , return_type);
        }
        if(return_type == 'b'){
            if(strcmp($<s_val>3, "false") == 0){
                fprintf(fp, "iconst_0\n");
            } else {
                fprintf(fp, "iconst_1\n");
            }
            fprintf(fp, "goto L_exit\n");
        }
        
        dump_symbol();
        
    }
    | StartBlock StatementList '}' 
    { 
        dump_symbol();
        if(is_if){
            is_if = false;
        }
        if(loop){
            fprintf(fp, "goto L_loop\n");
        }
    }
    | StartBlock BREAK Literal ';' NEWLINE '}' { dump_symbol();}
    
;
StartBlock
    : '{' { create_symbol(); } NEWLINE
;
DeclarationStmt
    : LET ID ':' Type { 
        insert_symbol($<s_val>4, $<s_val>2, "-", I, false ); 
        if(strcmp($<s_val>4, "i32") == 0){
            fprintf(fp, "ldc 0\n");
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>2, I));
        }
            
        else if(strcmp($<s_val>4, "f32") == 0){
            fprintf(fp, "ldc 0.000000\n");
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>2, I));
        }
            
        else if(strcmp($<s_val>4, "str") == 0){
            fprintf(fp, "ldc \"\\n\"\n");
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>2, I));
        }
    }
    | LET MUT ID ':' Type { 
        insert_symbol($<s_val>5, $<s_val>3, "-", I, true ); 
        if(strcmp($<s_val>5, "i32") == 0){
            fprintf(fp, "ldc 0\n");
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
        }
            
        else if(strcmp($<s_val>5, "f32") == 0){
            fprintf(fp, "ldc 0.000000\n");
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>3, I));
        }
            
        else if(strcmp($<s_val>5, "str") == 0){
            fprintf(fp, "ldc \"\\n\"\n");
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>3, I));
        }
    }
    | LET ID ':' Type '=' StatementList { 
        insert_symbol($<s_val>4, $<s_val>2, "-", I, false ); 
        if(strcmp($<s_val>4, "i32") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "f32") == 0)
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "str") == 0)
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "bool") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>2, I));
    }
    | LET MUT ID ':' Type '=' ExpressionStmt { 
        insert_symbol($<s_val>5, $<s_val>3, "-", I, true ); 
        if(strcmp($<s_val>5, "i32") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>5, "f32") == 0)
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>5, "str") == 0)
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>5, "bool") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
    }
    | LET ID ':' DeclareArrayStmt
    {
        is_array = true;
    }
     '=' ExpressionStmt 
    { 
        insert_symbol("array", $<s_val>2, "-", I, false );
        fprintf(fp, "astore_1\n");
    }
    | LET MUT ID '=' ExpressionStmt { 
        insert_symbol("i32", $<s_val>3, "-", I, true ); 
        fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
    }
    | LET ID '=' Literal 
    { 
        insert_symbol($<s_val>4, $<s_val>2, "-", I, false); 
        if(strcmp($<s_val>4, "i32") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "f32") == 0)
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "str") == 0)
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>2, I));
        else if(strcmp($<s_val>4, "bool") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>2, I));
    }
    | LET ID ':' Type '=' LoopStmt 
    {
        insert_symbol($<s_val>4, $<s_val>2, "-", I, false); 
        fprintf(fp, "L_loop_end:\n");
        if(strcmp($<s_val>4, "i32") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>4, "f32") == 0)
            fprintf(fp, "fstore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>4, "str") == 0)
            fprintf(fp, "astore %d\n", lookup_symbol_address($<s_val>3, I));
        else if(strcmp($<s_val>4, "bool") == 0)
            fprintf(fp, "istore %d\n", lookup_symbol_address($<s_val>3, I));
    }
;
DeclareArrayStmt
    : '[' DeclareArrayStmt ']' // multi-dimension
    | Type ';' Literal 
    {
        if(strcmp($<s_val>1, "i32") == 0)
            fprintf(fp, "newarray int\n");
    }
    
;
ArrayExpr
    : Literal ',' ArrayExpr
    | '[' Literal ',' ArrayExpr
    | Literal ']'
    | '&' ID  { lookup_symbol($<s_val>2, I); } '[' DotExpr ']'
;
AssignmentStmt
    : ExpressionStmt
    {
        strcpy(id_storage, id_temp);
    } 
    assign_op{
        
    } 
    ExpressionStmt 
    { 
        if(strcmp($<s_val>3, "ADD_ASSIGN") == 0){
            fprintf(fp, "%cadd\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "SUB_ASSIGN") == 0){
            fprintf(fp, "%csub\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "MUL_ASSIGN") == 0){
            fprintf(fp, "%cmul\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "DIV_ASSIGN") == 0){
            fprintf(fp, "%cdiv\n", $<s_val>1[0]);
        }
        else if(strcmp($<s_val>3, "REM_ASSIGN") == 0){
            fprintf(fp, "%crem\n", $<s_val>1[0]);
        }

        if(strcmp($<s_val>1, "str")==0){
            fprintf(fp, "astore %d\n", lookup_symbol_address(id_storage, I));
        }
        else if(strcmp($<s_val>1, "bool")==0){
            fprintf(fp, "istore %d\n", lookup_symbol_address(id_storage, I));
        }
        else{
            fprintf(fp, "%cstore %d\n", $<s_val>1[0], lookup_symbol_address(id_storage,I));
        }
    }
;
IFStmt
    : IFOpen NEWLINE
    {
        if(loop){
            fprintf(fp, "goto L_loop_end\n");
        }
        fprintf(fp, "L_if_%d: \n\n", label_number-1);
        fprintf(fp, "; ------if_end------\n");
    }
    | IFOpen NEWLINE ELSE 
    {
        fprintf(fp, "L_if_%d: \n", label_number-1);
        fprintf(fp, "; ------if_end------\n");
    }
    Block
    | IFOpen ELSE
    {
        fprintf(fp, "L_if_%d: \n", label_number-1);
        fprintf(fp, "; ------if_end------\n");
    } Block
    
;
IFOpen
    : IF
    {
        is_if = true;
        fprintf(fp, "; ------here is if start------\n");
    } 
    ExpressionStmt Block 
;
PrintStmt
    : PRINT '(' NEWLINE ExpressionStmt NEWLINE')' 
    {
        printf("PRINT %s\n", $<s_val>4);
        if(strcmp($<s_val>4, "bool") == 0){
            fprintf(fp, "; ----here is print -----\n");
            fprintf(fp, "ifne L_%d\n", label_number++);
            fprintf(fp, "ldc \"false\"\n");
            fprintf(fp, "goto L_%d\n", label_number++);
            fprintf(fp, "L_%d:\n", label_number-2);
            fprintf(fp, "ldc \"true\"\n");
            fprintf(fp, "L_%d:\n", label_number-1);
            fprintf(fp, "; -----------------------\n");
        }
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        fprintf(fp, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");

    }
    | PRINT '('  ExpressionStmt ')' 
    {
        printf("PRINT %s\n", $<s_val>3);
        if(strcmp($<s_val>4, "bool") == 0){
            fprintf(fp, "; ----here is print -----\n");
            fprintf(fp, "ifne L_%d\n", label_number++);
            fprintf(fp, "ldc \"false\"\n");
            fprintf(fp, "goto L_%d\n", label_number++);
            fprintf(fp, "L_%d:\n", label_number-2);
            fprintf(fp, "ldc \"true\"\n");
            fprintf(fp, "L_%d:\n", label_number-1);
            fprintf(fp, "; -----------------------\n");
        }
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        fprintf(fp, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
    | PRINTLN '(' ExpressionStmt ')'  
    {
        printf("PRINTLN %s\n", $<s_val>3);
        if(strcmp($<s_val>3, "bool") == 0){
            fprintf(fp, "; ----here is println -----\n");
            fprintf(fp, "ifne L_%d\n", label_number++);
            fprintf(fp, "ldc \"false\"\n");
            fprintf(fp, "goto L_%d\n", label_number++);
            fprintf(fp, "L_%d:\n", label_number-2);
            fprintf(fp, "ldc \"true\"\n");
            fprintf(fp, "L_%d:\n", label_number-1);
            fprintf(fp, "; -----------------------\n");
        }
        fprintf(fp, "getstatic java/lang/System/out Ljava/io/PrintStream;\n");
        fprintf(fp, "swap\n");
        if(strcmp($<s_val>3, "bool") == 0 || strcmp($<s_val>3, "str") == 0){
            fprintf(fp, "invokevirtual java/io/PrintStream/println(Ljava/lang/String;)V\n");
        }
        else if(is_array){
            
            fprintf(fp, "invokevirtual java/io/PrintStream/println(I)V\n");
            is_array = false;
        }
        else{
            fprintf(fp, "invokevirtual java/io/PrintStream/println(%c)V\n", toupper($<s_val>3[0]));
        }
        // fprintf(fp, "invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\n");
    }
;

WhileStmt
    : WHILE 
    {
        is_while = true;
        fprintf(fp, "L_while_%d: \n", label_number++);
    }
    ExpressionStmt
    {
        fprintf(fp, "; ------while_start------\n");
        // if(strstr($<s_val>2, "LES"))
        fprintf(fp, "iflt L_while_%d\n", label_number++);
        fprintf(fp, "goto L_while_%d\n", label_number++);
        fprintf(fp, "L_while_%d: \n", label_number-2);//true
    } 
    Block
    {
        fprintf(fp, "goto L_while_%d\n", label_number-3);
        fprintf(fp, "L_while_%d: \n\n", label_number-1);
        fprintf(fp, "; ------while_end------\n");
    }
;
ForStmt
    : FOR
    {
        fprintf(fp, "iconst_0\n");
        fprintf(fp, "istore_1\n");
        fprintf(fp, "aload_0\n");
        fprintf(fp, "arraylength\n");
        fprintf(fp, "istore_2\n");

        fprintf(fp, "loop:\n");
        fprintf(fp, "iload_1\n");
        fprintf(fp, "iload_2\n");
        fprintf(fp, "if_icmpge end\n");
        fprintf(fp, "aload_0\n");
        fprintf(fp, "iload_1\n");
        fprintf(fp, "iaload\n");
    }
    ID IN ID 
    {
        lookup_symbol($<s_val>4, I); 
    }
    StartBlock { insert_symbol("i32", $<s_val>2, "-", Fo, false); }  StatementList  '}'
    { 
        fprintf(fp, "iinc 1 1\n");
        fprintf(fp, "goto loop\n");
        fprintf(fp, "end:\n");
        dump_symbol() ;
    }
;
LoopStmt
    : LOOP 
    {
        fprintf(fp, "L_loop:\n");
        loop = true;
    }
    Block
    {

    }
;

ExpressionStmt
    : LogicalORExpr {$$ = $1;}
;
LogicalORExpr
    : LogicalANDExpr LOR LogicalANDExpr
    {
        $$ = "bool";
        printf("LOR\n");
        fprintf(fp, "ior\n");
    }
    | LogicalANDExpr { $$ = $1; }
;
LogicalANDExpr
    : ComparisonExpr LAND ComparisonExpr
    {
        $$ = "bool"; 
        printf("LAND\n");
        fprintf(fp, "iand\n");
    }
    | ComparisonExpr { $$ = $1; }
;
ComparisonExpr
    : AdditionExpr cmp_op AdditionExpr
    {
        if(strcmp($<s_val>1, $<s_val>3) != 0){
            printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", yylineno+1, $<s_val>2, $<s_val>1, $<s_val>3);
        }
        else{
            if(strcmp($<s_val>1, "i32") == 0){
                
                fprintf(fp, "isub\n");
            }
                
            else if(strcmp($<s_val>1, "f32") == 0)
                fprintf(fp, "fcmpl\n");

            if(strcmp($<s_val>2, "GTR") == 0){
                fprintf(fp, "; --- here is greater than ---\n");
                fprintf(fp, "ifgt L%d\n", label_number++);
                fprintf(fp, "iconst_0\n");
                fprintf(fp, "goto L%d\n", label_number++);
                fprintf(fp, "L%d:\n", label_number-2);
                fprintf(fp, "iconst_1\n");
                fprintf(fp, "L%d:\n", label_number-1);
                fprintf(fp, "; ----------greater end------------\n");
            }
               
            else if(strcmp($<s_val>2, "LES") == 0){
                fprintf(fp, "; --- here is less than ---\n");
                fprintf(fp, "iflt L%d\n", label_number++);
                fprintf(fp, "iconst_0\n");
                fprintf(fp, "goto L%d\n", label_number++);
                fprintf(fp, "L%d:\n", label_number-2);
                fprintf(fp, "iconst_1\n");
                fprintf(fp, "L%d:\n", label_number-1);
                fprintf(fp, "; --------less end--------\n");
            }
            else if(strcmp($<s_val>2, "EQL") == 0){
                fprintf(fp, "; --- here is equal to ---\n");
                fprintf(fp, "ifeq L_if_%d\n", label_number++);
                fprintf(fp, "goto L_if_%d\n", label_number++);
                fprintf(fp, "L_if_%d: \n\n", label_number-2);//true
                if(is_if == false)
                    fprintf(fp, "\nL_if_%d: \n", label_number-1);
                else 
                    is_if = false;
                fprintf(fp, "; ---------equal end---------\n");
            }
        }
        $$ = "bool";
        printf("%s\n", $<s_val>2);
    }
    | AdditionExpr { $$ = $1; }
;
AdditionExpr
    : MultiplicationExpr add_op MultiplicationExpr
    {
        
        printf("%s\n", $<s_val>2);
        $$ = $1; 
        fprintf(fp, "%c%s\n", tolower($<s_val>1[0]), $<s_val>2);
    }
    | AdditionExpr add_op MultiplicationExpr
    {
        printf("%s\n", $<s_val>2);
        $$ = $1; 
        fprintf(fp, "%c%s\n", tolower($<s_val>1[0]), $<s_val>2);
    }
    | MultiplicationExpr { $$ = $1; }
;

MultiplicationExpr
    : UnaryExpr mul_op UnaryExpr
    {
        $$ = $1;
        printf("%s\n", $<s_val>2);
        fprintf(fp, "%c%s\n", tolower($<s_val>1[0]), $<s_val>2);

    }
    | UnaryExpr shift_op UnaryExpr
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
    : unary_op UnaryExpr 
    { 
        $$ = $2; 
        printf("%s\n", $<s_val>1); 
        if(strcmp($<s_val>1, "not") == 0) // if !a, a is 1 => ans = 0
        {
            fprintf(fp, "; here is do not\n");
            fprintf(fp, "iconst_1\n");// let x = 1
            if(strcmp($<s_val>2, "true") == 0)
                fprintf(fp, "iconst_1\n");
            else if(strcmp($<s_val>2, "false") == 0)
                fprintf(fp, "iconst_0\n");
            fprintf(fp, "ixor\n");// if a is 1, x xor a = 0; if a is 0, x xor a = 1
            fprintf(fp, "; --------------\n");
            $$ = "bool";
        }
        else if(strcmp($<s_val>1, "neg") == 0)
            fprintf(fp, "%cneg\n", $<s_val>2[0]);
    }
    | ArrayExpr
    | Operand 
    { 
        $$ = $1; 
        if(strcmp($<s_val>1, "true")==0){
            fprintf(fp, "iconst_1\n");
            $$ = "bool";
        }
        else if(strcmp($<s_val>1, "false")==0){
            fprintf(fp, "iconst_0\n");
            $$ = "bool";
        }
        
    }
;

DotExpr
    : DOTDOT { printf("DOTDOT\n"); } Literal 
    | DOtOpen { printf("DOTDOT\n"); }
    | DOtOpen  { printf("DOTDOT\n"); } Literal
;
DOtOpen
    : Literal DOTDOT
Operand
    : Literal { $$ = $1; }
    | Literal AS { casting = true; } Type { $$ = $<s_val>4; }// change_type
    | ID { $$ = lookup_symbol($<s_val>1, I) ;}
    | '(' ExpressionStmt ')' { $$ = $2; } // call function
    | ID '[' INT_LIT ']' // array
    { 
        fprintf(fp, "aload_1\n");
        fprintf(fp, "iconst_%d\n", $<i_val>3);
        fprintf(fp, "iaload\n");
        $$ = lookup_symbol($<s_val>1, I); 
        printf("INT_LIT %d\n", $<i_val>3);
        is_array = true;
    } 
    | ID AS // change_type
    {
        casting = true;
        lookup_symbol($<s_val>1, I);
    }
    Type 
    {
        $$ = $<s_val>4;
    }
    | ID '(' ID ',' ID ')' // call function
    {
        lookup_symbol($<s_val>3, P); // parameter
        lookup_symbol($<s_val>5, P); // parameter
        has_return = true;
        return_type = 'b';
        lookup_symbol($<s_val>1, Fu); // function name
        fprintf(fp, "ifeq L_if_%d\n", label_number++);
        fprintf(fp, "goto L_if_%d\n", label_number++);
        fprintf(fp, "L_if_%d: \n", label_number-2);
    }
;
Literal
    : INT_LIT { $$ = "i32"; printf("INT_LIT %d\n", $<i_val>1); 
        if(is_while){
            is_while = false;
            fprintf(fp, "ldc %d\n", $<i_val>1);
        }
        else if(is_array){
            fprintf(fp, "dup\n");
            fprintf(fp, "iconst_%d\n", ptr_array++);
            fprintf(fp, "bipush %d\n", $<i_val>1);
            fprintf(fp, "iastore\n");
        }
        else
            fprintf(fp, "ldc %d\n", $<i_val>1);
    }
    | FLOAT_LIT { $$ = "f32"; printf("FLOAT_LIT %f\n", $<f_val>1);fprintf(fp, "ldc %f\n", $<f_val>1);}
    | '"' STRING_LIT '"' { $$ = "str"; printf("STRING_LIT \"%s\"\n", $<s_val>2);fprintf(fp, "ldc \"%s\"\n", $<s_val>2);}
    | '"''"' { $$ = "str"; printf("STRING_LIT \"\"\n");fprintf(fp, "ldc \"\"\n");}
    | TRUE { $$ = "true"; printf("bool TRUE\n");}
    | FALSE { $$ = "false"; printf("bool FALSE\n");}
;

assign_op
    : '=' {$$ = "ASSIGN";}
    | ADD_ASSIGN {$$ = "ADD_ASSIGN";}
    | SUB_ASSIGN {$$ = "SUB_ASSIGN";}
    | MUL_ASSIGN {$$ = "MUL_ASSIGN";}
    | DIV_ASSIGN {$$ = "DIV_ASSIGN";}
    | REM_ASSIGN {$$ = "REM_ASSIGN";}
;

cmp_op 
    : EQL { $$ = "EQL"; }
    | NEQ { $$ = "NEQ"; }
    | '<' { $$ = "LSS"; }
    | LEQ { $$ = "LEQ"; }
    | '>' { $$ = "GTR"; }
    | GEQ { $$ = "GEQ"; }
;

add_op 
    : '+' { $$ = "add"; }
    | '-' { $$ = "sub"; }
;

mul_op 
    : '*' { $$ = "mul"; }
    | '/' { $$ = "div"; }
    | '%' { $$ = "rem"; }
;

shift_op
    : LSHIFT { $$ = "ishl"; }
    | RSHIFT { $$ = "iushr"; }
;

unary_op 
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
            fprintf(fp, "f2i\n");
        } 
    }
	| FLOAT		
    { 
        $$ = "f32";
        if(casting){
            casting = false;
            printf("i2f\n");
            fprintf(fp, "i2f\n");
        } 
    }
	| '&'STR	    { $$ = "str"; }
	| BOOL		{ $$ = "bool"; }
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
        printf("file %s doesn't exists or cannot be opened\n", argv[1]);
        exit(1);
    }
    /* hw3 */
    char *bytecode_filename = "hw3.j";
    fp = fopen(bytecode_filename, "w");
    fprintf(fp,".source hw3.j\n");
    fprintf(fp,".class public Main\n");
    fprintf(fp,".super java/lang/Object\n");

    create_symbol();
    yylineno = 0;
    yyparse();
    dump_symbol();

	printf("Total lines: %d\n", yylineno);
    fclose(fp);
    fclose(yyin);
     if (g_has_error) {
        remove(bytecode_filename);
    }
    yylex_destroy();
    return 0;
}

static void create_symbol() {
    global_scope ++;
    printf("> Create symbol table (scope level %d)\n", global_scope);
    
    struct table *tmp = malloc(sizeof(struct symbol));
    tmp -> scope = global_scope;
    tmp -> prev = current_table;
    tmp -> head = NULL;
    current_table = tmp;
    
}

static void insert_symbol(char* type, char* name, char* func_sig, int mark_var, bool has_mut) {// if mark_var = 0, function; else if mark_var = 1, parameter, else if mark_var == 2, id, else(foreach), mark_var == 3
    struct symbol *tail = NULL;
    struct table *first = current_table;
    bool empty = false;
    if(mark_var == Fu){ // function
        while(first-> prev != NULL) // is not the head of table
            first = first -> prev;
        /* after find the head of table */
        if(first -> head != NULL){ // not empty -> add to tail
            tail = first -> head;
            while(tail -> next != NULL)
                tail = tail->next;
        }


        else // empty
            empty = true;
    }
    else{
        if(first -> head != NULL){
            tail = current_table -> head;
            while(tail -> next !=NULL){
                tail = tail->next;
            }
        }
        else
            empty = true;
    }
    /* to this point, first and tail is moved to the right position */
        /* function: first -> head of table, tail -> the tail of first */
        /* parameter: first -> current of table, tail -> the tail of first */
    struct symbol *new = malloc(sizeof(struct symbol));
    // next
    new -> next = NULL;
    // name
    new -> name = strdup(name);// 和strcpy不太一樣，可以直接把要複製的內容給未經過初始化或分配空間的指標，相當於strlen + 分配空間 + strcpy  
    // mut
    if(mark_var == Fu)
        new -> mut = -1;
    else if(has_mut)
        new -> mut = 1;
    else 
        new -> mut = 0;
    // type
    new -> type = strdup(type);
    // addr and lineno 
    if(mark_var == Fu){// function
        new -> lineno = yylineno + 1;
        new -> addr = -1;
    }
    else if(mark_var == P){// parameter
        new -> lineno = yylineno + 1;
        new -> addr = addr;
        addr ++;
    }
    else if(mark_var == I) { // id
        new -> lineno = yylineno + 1 ;
        new -> addr = addr;
        addr ++;
    }
    else{ // foreach
        new -> lineno = yylineno;
        new -> addr = addr;
        addr ++;
    }
    // func_sig
    new -> func_sig = strdup(func_sig);
    // index
    if(!empty){
        new -> index = (tail -> index) + 1;
        tail -> next = new;
    }
    else{
        new -> index = 0;
        first -> head = new;
    }
    // print
    if(mark_var == I || mark_var == Fo)// id and foreach
        printf("> Insert `%s` (addr: %d) to scope level %d\n", name, new -> addr, global_scope);
}

static int lookup_symbol_address(char *name, int mark_var) { // if mark_var = 0, function; else if mark_var = 1, parameter, else(id) is 2
    struct table *t = current_table;
    struct symbol *s = NULL;
    while(t!=NULL){
        s = t -> head;
        while(s!= NULL){
            if(strcmp(s-> name, name) == 0){
                if(mark_var == Fu)// function
                {
                    if(has_return){
                        strcat(func_para, ")");
                        printf("call: %s%s%c\n", s -> name, func_para, toupper(return_type));
                        has_return = false;
                        if(return_type == 'b')
                            s -> func_sig = strcat(func_para, "B");
                        return s -> func_sig  ;
                    }
                    else{
                        printf("call: %s%s\n", s -> name, s -> func_sig);
                        return s -> func_sig;
                    }
                }
                    
                else if(mark_var != Fo){// parameter and idZ
                    printf("IDENT (name=%s, address=%d)\n", s->name, s->addr);
                    return s-> addr;
                }
                else // foreach
                    return s-> addr;
            }
            else// name is not same
                s = s->next;
        }
        t = t-> prev;
    }
    /* if it's not return until this step, it is represent the symbol is an error */
    printf("error:%d: undefined: %s\n", yylineno+1, name);
    return "undefined";
}

static char *lookup_symbol(char *name, int mark_var) { // if mark_var = 0, function; else if mark_var = 1, parameter, else(id) is 2
    struct table *t = current_table;
    struct symbol *s = NULL;
    while(t!=NULL){
        s = t -> head;
        while(s!= NULL){
            if(strcmp(s-> name, name) == 0){
                if(mark_var == Fu)// function
                {
                    if(return_type == 'v')
                        fprintf(fp, "invokestatic Main/%s%s\n", s -> name, s -> func_sig);
                    else if(return_type == 'b')
                        fprintf(fp, "invokestatic Main/%s%s%c\n", s -> name, s -> func_sig, toupper(return_type));
                    /* printf("call: %s\n", s -> name); */
                    if(has_return){
                        strcat(func_para, ")");
                        printf("call: %s%s%c\n", s -> name, func_para, toupper(return_type));
                        has_return = false;
                        if(return_type == 'b')
                            s -> func_sig = strcat(func_para, "B");
                        return s -> func_sig  ;
                    }
                    else{
                        printf("call: %s%s\n", s -> name, s -> func_sig);
                        return s -> func_sig;
                    }
                    
                }
                    
                else if(mark_var != Fo){// parameter or id
                    printf("IDENT (name=%s, address=%d)\n", s->name, s->addr);
                    if(strcmp(s->type, "i32") == 0 || strcmp(s->type, "bool") == 0)
                        fprintf(fp, "iload %d\n", s-> addr);
                    else if(strcmp(s->type, "f32") == 0)
                        fprintf(fp, "fload %d\n", s-> addr);
                    else if(strcmp(s->type, "str") == 0)
                        fprintf(fp, "aload %d\n", s-> addr);
                    strcpy(id_temp, s->name);
                    return s-> type;
                }
                else
                    return s-> type;
            }
            else// name is not same
                s = s->next;
        }
        t = t-> prev;
    }
    /* if it's not return until this step, it is represent the symbol is an error */
    printf("error:%d: undefined: %s\n", yylineno+1, name);
    g_has_error = true;
    return "undefined";
}


static void dump_symbol() {
    printf("\n> Dump symbol table (scope level: %d)\n", current_table -> scope);
    printf("%-10s%-10s%-10s%-10s%-10s%-10s%-10s\n",
        "Index", "Name", "Mut","Type", "Addr", "Lineno", "Func_sig");
    
    struct symbol *tmp = current_table-> head;
    while(tmp!= NULL){
        printf("%-10d%-10s%-10d%-10s%-10d%-10d%-10s\n",
            tmp-> index, tmp-> name, tmp-> mut, tmp-> type, tmp-> addr, tmp-> lineno, tmp-> func_sig);
        tmp = tmp->next;
    }
    current_table = current_table-> prev;
    global_scope--;
}

static void build_func_para(char *para) {
    char tmp = toupper(para[0]);
    strcat(func_para, &tmp);
}
