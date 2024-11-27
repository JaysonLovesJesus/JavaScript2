enum TokenKind {
    TT_UNKNOWN,

    // keywords
    KW_ENUM,
    KW_LET,
    KW_EXPORT,
    KW_FN,
    KW_IF,
    KW_ELSE,
    KW_RETURN,

    // reserved keywords
    KW_CONST,
    KW_FUNCTION,
    KW_WHILE,
    KW_BREAK,
    KW_CONTINUE,

    // punctuators
    PP_LPAREN,
    PP_RPAREN,
    PP_LBRACK,
    PP_RBRACK,
    PP_LBRACE,
    PP_RBRACE,
    PP_DOT,
    PP_COLON,
    PP_COMMA,
    PP_SEMICOLON,

    // operators
    OP_ASS,
    OP_ADD,
    OP_SUB,
    OP_MUL,
    OP_DIV,
    OP_NEW,
    OP_OR,
    OP_AND,
    OP_NOT,
    // ??
    OP_LT,
    OP_LTE,
    OP_GT,
    OP_GTE,
    OP_EQUAL,
    OP_NEQUAL,
    OP_BIN_OR,
    OP_BIN_AND,
    OP_ADD_ADD,
    OP_SUB_SUB,

    // token kinds
    TT_undefined,
    TT_STRING,
    TT_NUMBER,
    TT_BOOLEAN,
    TT_IDENTIFIER,

    // node kinds
    NN_PROGRAM,
    NN_IF,
    NN_LET,
    NN_CONST,
    NN_EXPORT,
    NN_FUNCTION,
    NN_ENUM,
    NN_ENUM_ITEM,
    NN_ENUM_EXPRESSION,
    NN_UNARY_PREFIX_EXPRESSION,
    NN_UNARY_POSTFIX_EXPRESSION,
    NN_BINARY_EXPRESSION,
    NN_MEMBER_EXPRESSION,
    NN_COMPUTED_MEMBER_EXPRESSION,
    NN_OBJECT_EXPRESSION,
    NN_OBJECT_PROPERTY,
    NN_ARRAY_EXPRESSION,
    NN_ARRAY_ELEMENT,
    NN_CALL_EXPRESSION,
    NN_WHILE,
    NN_RETURN,
    NN_BREAK,
    NN_CONTINUE,
    NN_LITERAL,
    NN_STRING_LITERAL,
    NN_INOUT
}

// HELPER FUNCTIONS
function is_blank(cc: number): boolean {
    return (
        cc == 9 ||
        cc == 11 ||
        cc == 12 ||
        cc == 32 ||
        cc == 160
    );
}

function is_quote(cc: number): boolean {
    return (
        cc == 39 ||
        cc == 34
    );
}

function is_alpha(cc: number): boolean {
    return (
        cc >= 65 && cc <= 90 ||
        cc >= 97 && cc <= 122 ||
        cc == 95
    );
}

function is_number(cc: number): boolean {
    return cc >= 48 && cc <= 57;
}


function is_binary_operator(token: Token): boolean {
    const kind = token.kind;
    return (
        (kind == TokenKind.OP_ASS ||
        kind == TokenKind.OP_ADD ||
        kind == TokenKind.OP_SUB ||
        kind == TokenKind.OP_MUL ||
        kind == TokenKind.OP_DIV ||
        kind == TokenKind.OP_OR ||
        kind == TokenKind.OP_AND ||
        kind == TokenKind.OP_NOT ||
        kind == TokenKind.OP_LT ||
        kind == TokenKind.OP_LTE ||
        kind == TokenKind.OP_GT ||
        kind == TokenKind.OP_GTE ||
        kind == TokenKind.OP_EQUAL ||
        kind == TokenKind.OP_NEQUAL ||
        kind == TokenKind.OP_BIN_OR ||
        kind == TokenKind.OP_BIN_AND) &&
        !is_unary_prefix_operator(token)
    );
}

function is_unary_prefix_operator(token: Token): boolean {
    const kind = token.kind;
    return (
        kind == TokenKind.OP_NEW ||
        kind == TokenKind.OP_NOT ||
        kind == TokenKind.OP_ADD_ADD ||
        kind == TokenKind.OP_SUB_SUB
    );
}

function is_unary_postfix_operator(token: Token): boolean {
    const kind = token.kind;
    return (
        kind == TokenKind.OP_ADD_ADD ||
        kind == TokenKind.OP_SUB_SUB
    );
}

function is_literal(token: Token): boolean {
    const kind = token.kind;
    return (
        kind == TokenKind.TT_undefined ||
        kind == TokenKind.TT_STRING ||
        kind == TokenKind.TT_NUMBER ||
        kind == TokenKind.TT_BOOLEAN ||
        kind == TokenKind.TT_IDENTIFIER
    );
}

function is_punctuator_char(ch: string): boolean {
    return (
        ch == "(" ||
        ch == ")" ||
        ch == "[" ||
        ch == "]" ||
        ch == "{" ||
        ch == "}" ||
        ch == "." ||
        ch == ":" ||
        ch == "," ||
        ch == ";" ||
        ch == "*" ||
        ch == "/"
    );
}

function is_operator_char(ch: string): boolean {
    return (
        ch == "+" ||
        ch == "-" ||
        ch == "!" ||
        ch == "=" ||
        ch == "|" ||
        ch == "&" ||
        ch == ">" ||
        ch == "<"
    );
}

function is_operator(str: string): boolean {
    if (str.length == 1) {
        return (is_operator_char(str));
    }
    return (
        str == "++" ||
        str == "--" ||
        str == "==" ||
        str == "!=" ||
        str == "||" ||
        str == "&&" ||
        str == ">=" ||
        str == "<="
    );
}

function process_token(tokens: Token[], value: string, line: number, column: number) {
    let kind = TokenKind.TT_UNKNOWN;
    // keywords
    if (value == "enum") kind = TokenKind.KW_ENUM;
    else if (value == "let") kind = TokenKind.KW_LET;
    else if (value == "const") kind = TokenKind.KW_CONST;
    else if (value == "export") kind = TokenKind.KW_EXPORT;
    else if (value == "function") kind = TokenKind.KW_FUNCTION;
    else if (value == "if") kind = TokenKind.KW_IF;
    else if (value == "else") kind = TokenKind.KW_ELSE;
    else if (value == "while") kind = TokenKind.KW_WHILE;
    else if (value == "break") kind = TokenKind.KW_BREAK;
    else if (value == "continue") kind = TokenKind.KW_CONTINUE;
    else if (value == "return") kind = TokenKind.KW_RETURN;
    // boolean
    else if (value == "true" || value == "false") kind = TokenKind.TT_BOOLEAN;
    // undefined
    else if (value == "undefined") kind = TokenKind.TT_undefined;
    // punctuators
    else if (value == "(") kind = TokenKind.PP_LPAREN;
    else if (value == ")") kind = TokenKind.PP_RPAREN;
    else if (value == "[") kind = TokenKind.PP_LBRACK;
    else if (value == "]") kind = TokenKind.PP_RBRACK;
    else if (value == "{") kind = TokenKind.PP_LBRACE;
    else if (value == "}") kind = TokenKind.PP_RBRACE;
    else if (value == ".") kind = TokenKind.PP_DOT;
    else if (value == ":") kind = TokenKind.PP_COLON;
    else if (value == ",") kind = TokenKind.PP_COMMA;
    else if (value == ";") kind = TokenKind.PP_SEMICOLON;
    // operators
    else if (value == "!") kind = TokenKind.OP_NOT;
    else if (value == "=") kind = TokenKind.OP_ASS;
    else if (value == "+") kind = TokenKind.OP_ADD;
    else if (value == "-") kind = TokenKind.OP_SUB;
    else if (value == "*") kind = TokenKind.OP_MUL;
    else if (value == "/") kind = TokenKind.OP_DIV;
    else if (value == "<") kind = TokenKind.OP_LT;
    else if (value == "<=") kind = TokenKind.OP_LTE;
    else if (value == ">") kind = TokenKind.OP_GT;
    else if (value == ">=") kind = TokenKind.OP_GTE;
    else if (value == "|") kind = TokenKind.OP_BIN_OR;
    else if (value == "&") kind = TokenKind.OP_BIN_AND;
    else if (value == "==") kind = TokenKind.OP_EQUAL;
    else if (value == "!=") kind = TokenKind.OP_NEQUAL;
    else if (value == "||") kind = TokenKind.OP_OR;
    else if (value == "&&") kind = TokenKind.OP_AND;
    else if (value == "++") kind = TokenKind.OP_ADD_ADD;
    else if (value == "--") kind = TokenKind.OP_SUB_SUB;
    else if (value == "new") kind = TokenKind.OP_NEW;
    else kind = TokenKind.TT_IDENTIFIER;

    const token = create_token(kind, value, line, column-value.length);
    tokens.push(token);
    return (token);
}

// SCANNER
interface Token {
    kind: TokenKind,
    value: string,
    line: number,
    column: number,
    is_char?: boolean,
    is_parameter?: boolean,
}
function create_token(
    kind: TokenKind,
    value: string,
    line: number,
    column: number
): Token {
    const token = {
        kind: kind,
        value: value,
        line: line,
        column: column
    };
    return (token);
};

function scan(str: string) {
    let ii = -1;
    let line = 1;
    let column = 0;
    let length = str.length;

    let tokens: Token[] = [];

    function next() {
        ii++;
        column++;
    };

    // placed here to have correct context to next()
    function processOperator(ch: string, second: string, line: number, column: number) {
        if (second && is_operator(ch + second)) {
            next();
            process_token(tokens, ch + second, line, column);
        } else if (is_operator(ch)) {
            process_token(tokens, ch, line, column);
        }
    };

    while (true) {
        next();
        let ch = str.charAt(ii);
        let cc = str.charCodeAt(ii);
        // blank
        if (is_blank(cc)) {
            continue;
        }
        if (cc == 10) {
            line ++;
            column = 0;
            continue;
        }
        // alpha
        if (is_alpha(cc)) {
            let start = ii;
            while (true) {
                if (!is_alpha(cc) && !is_number(cc)) {
                    ii--;
                    column--;
                    break;
                }
                next();
                cc = str.charCodeAt(ii);
            };
            let content = str.slice(start, ii+1);
            process_token(tokens, content, line, column);
            continue;
        }
        // number
        if (is_number(cc) || cc == 45 && is_number(str.charCodeAt(ii+1))) {
            let start = ii;
            while (true) {
                if (!is_number(cc) && cc != 45) {
                ii --;
                column --;
                break;
                }
                next();
                cc = str.charCodeAt(ii);
            };
            let content = str.slice(start, ii+1);
            let token = create_token(TokenKind.TT_NUMBER, content, line, column);
            tokens.push(token);
            continue;
        }
        // string
        if (is_quote(cc)) {
            let start = ii;
            let begin = cc;
            while (true) {
                next();
                cc = str.charCodeAt(ii);
                // break on next matching quote
                if (is_quote(cc) && cc == begin) {
                    break;
                }
            };
            let content = str.slice(start+1, ii);
            let token = create_token(TokenKind.TT_STRING, content, line, column);
            token.is_char = content[0] == "'";
            tokens.push(token);
            continue;
        }
        if (ch == "/") {
            if (str.charAt(ii + 1) == "/") {
                while (true) {
                    if (cc == 10) {
                        column = 0;
                        line++;
                        break;
                    }
                    next();
                    cc = str.charCodeAt(ii);
                };
            }
            continue;
        }
        if (is_punctuator_char(ch)) {
            let content = str.slice(ii, ii+1);
            process_token(tokens, content, line, column);
            continue;
        }
        if (is_operator_char(ch)) {
            let second = str.slice(ii+1, ii+2);
            processOperator(ch, second, line, column);
            continue;
        }

        if (ii >= length) {
            break;
        }
    };
    return (tokens);
};



// AST NODES
interface Node {
    kind: TokenKind;
    context?: Scope; // Scope associated with this node
}

interface ProgramNode extends Node {
    kind: TokenKind.NN_PROGRAM,
    body: Node[];
}

interface ExportNode extends Node {
    kind: TokenKind.NN_EXPORT,
    init: Node | undefined;
}

interface LetNode extends Node {
    kind: TokenKind.NN_LET,
    id: string;
    init: Node | undefined;
    isLaterReference?: boolean; // Marker to use {$iov: value} syntax
}

interface ConstNode extends Node {
    kind: TokenKind.NN_CONST,
    id: string;
    init: Node | undefined;
}

interface FunctionNode extends Node {
    kind: TokenKind.NN_FUNCTION,
    id?: string;
    parameter: Token[]; // Use Token interface for params
    body: Node[];
}

interface ReturnNode extends Node {
    kind: TokenKind.NN_RETURN,
    argument: Node | undefined;
}

interface IfNode extends Node {
    kind: TokenKind.NN_IF,
    condition: Node | undefined;
    consequent: Node[]; // Consequent can be a block of statements
    alternate: Node | undefined; // Alternate can be another if statement or undefined
}

interface WhileNode extends Node {
    kind: TokenKind.NN_WHILE,
    condition: Node | undefined;
    body: Node[];
}


interface EnumNode extends Node {
    kind: TokenKind.NN_ENUM;
    name: string;
    body: EnumItemNode[];
}

interface EnumItemNode extends Node {
    kind: TokenKind.NN_ENUM_ITEM;
    name: string;
    init: { value: number };
}

interface EnumExpressionNode extends Node {
    kind: TokenKind.NN_ENUM_EXPRESSION;
    value: { value: number } | undefined; // Store the resolved numeric value
}

interface UnaryPrefixExpressionNode extends Node {
    kind: TokenKind.NN_UNARY_PREFIX_EXPRESSION;
    operator: string;
    value: Node | undefined;
}

interface UnaryPostfixExpressionNode extends Node {
    kind: TokenKind.NN_UNARY_POSTFIX_EXPRESSION,
    operator: string,
    value: Node | undefined;
}

interface BinaryExpressionNode extends Node {
    kind: TokenKind.NN_BINARY_EXPRESSION;
    left: Node | undefined;
    right: Node | undefined;
    operator: string;
}

interface MemberExpressionNode extends Node {
    kind: TokenKind.NN_MEMBER_EXPRESSION;
    parent: Node | undefined;
    member: Node | undefined;
}

interface ComputedMemberExpressionNode extends Node {
    kind: TokenKind.NN_COMPUTED_MEMBER_EXPRESSION;
    parent: Node | undefined;
    member: Node | undefined;
}


interface CallExpressionNode extends Node {
    kind: TokenKind.NN_CALL_EXPRESSION;
    callee: Node;
    parameter: (Node)[]; // Array of arguments
}

interface BreakNode extends Node {
    kind: TokenKind.NN_BREAK;
}


interface ContinueNode extends Node {
    kind: TokenKind.NN_CONTINUE;
}

interface LiteralNode extends Node {
    kind: TokenKind.NN_LITERAL;
    type: TokenKind;
    value: string;
}

interface StringLiteralNode extends Node {
    kind: TokenKind.NN_STRING_LITERAL,
    type: TokenKind,
    value: string;
    isChar: boolean;
}

interface ObjectExpressionNode extends Node {
    kind: TokenKind.NN_OBJECT_EXPRESSION;
    properties: ObjectPropertyNode[];
}


interface ObjectPropertyNode extends Node {
    kind: TokenKind.NN_OBJECT_PROPERTY;
    id: Node; // Key can be more complex than just a literal
    value: Node | undefined;
}

interface ArrayExpressionNode extends Node {
    kind: TokenKind.NN_ARRAY_EXPRESSION;
    elements: ArrayElementNode[];
}

interface ArrayElementNode extends Node {
    kind: TokenKind.NN_ARRAY_ELEMENT,
    value: Node; // Array elements can be any expression
}

type ASTNode =
    ProgramNode |
    ExportNode |
    LetNode |
    ConstNode |
    FunctionNode |
    ReturnNode |
    IfNode |
    WhileNode |
    EnumNode |
    EnumItemNode |
    EnumExpressionNode |
    UnaryPrefixExpressionNode |
    UnaryPostfixExpressionNode |
    BinaryExpressionNode |
    MemberExpressionNode |
    ComputedMemberExpressionNode |
    CallExpressionNode |
    BreakNode |
    ContinueNode |
    LiteralNode |
    StringLiteralNode |
    ObjectExpressionNode |
    ObjectPropertyNode |
    ArrayExpressionNode |
    ArrayElementNode;


// SCOPE
interface Scope {
    node: ASTNode | undefined;
    parent: Scope | undefined;
    symbols: Record<string, ASTNode>;
    resolve: (id: string) => ASTNode | undefined;
    register: (id: string, node: ASTNode) => void;
}
let scope: Scope;
function Scope(this: Scope) {
    // this.node = undefined;
    // this.parent = undefined;
    this.symbols = {};
    this.resolve = function(id: string): ASTNode | undefined {
        if (this.symbols[id]) {
            return (this.symbols[id]);
        } else {
            // recursively search symbol inside parent
            if (this.parent) {
                return (this.parent.resolve(id));
            }
        }
        return (undefined);
    };
    this.register = function(id: string, node: ASTNode) {
        this.symbols[id] = node;
    };
};

function push_scope(node: Partial<ASTNode>) {
    let scp = new (Scope as any)();
    scp.node = node;
    scp.parent = scope;
    node.context = scp;
    scope = scp;
};

function pop_scope() {
    if (scope != undefined && scope.parent) {
        scope = scope.parent;
    }
};

// PARSER
let pindex = 0;
let tokens: Token[];
let current: Token;

function parse(tkns: Token[]): ASTNode {
    tokens = tkns;
    pindex = -1;
    next();
    let node: ProgramNode = {
        kind: TokenKind.NN_PROGRAM,
        body: [],
    };
    push_scope(node);
    node.body = parse_statement_list();
    return (node as ASTNode);
};

function peek(kind: TokenKind): boolean {
    return (current && current.kind == kind);
};

function next(): void {
    pindex++;
    current = tokens[pindex];
};

function expect(kind: TokenKind): void {
    if (current.kind != kind) {
        console.error("Expected " + kind + " but got " + current.kind + " in " + current.line + ":" + current.column);
    } else {
        next();
    }
};

function expect_identifier(): void {
    if (current.kind != TokenKind.TT_IDENTIFIER) {
        console.error("Expected " + TokenKind.TT_IDENTIFIER + ":identifier but got " + current.kind + ":" + current.value);
    }
};

function eat(kind: TokenKind): boolean {
    if (peek(kind)) {
        next();
        return (true);
    }
    return (false);
};

function parse_statement_list(): ASTNode[] {
    let list: ASTNode[] = [];
    while (true) {
        if (!current) break;
        if (peek(TokenKind.PP_RBRACE)) break;
        let node = parse_statement();
        if (!node) break;
        list.push(node);
    };
    return (list);
};

function parse_statement(): ASTNode {
    let node: Partial<ASTNode>;
    if (peek(TokenKind.KW_LET)) {
        node = parse_variable_declaration(TokenKind.NN_LET);
    } else if (peek(TokenKind.KW_CONST)) {
        node = parse_variable_declaration(TokenKind.NN_CONST);
    } else if (peek(TokenKind.KW_FUNCTION)) {
        node = parse_function_declaration();
    } else if (peek(TokenKind.KW_RETURN)) {
        node = parse_return_statement();
    } else if (peek(TokenKind.KW_IF)) {
        node = parse_if_statement();
    } else if (peek(TokenKind.KW_WHILE)) {
        node = parse_while_statement();
    } else if (peek(TokenKind.KW_ENUM)) {
        node = parse_enum_declaration();
    } else if (peek(TokenKind.KW_EXPORT)) {
        node = parse_export();
    } else {
        node = parse_expression();
        if (node == undefined) {
            console.error("Unknown node kind " + current.value + " in " + current.line + ":" + current.column);
        }
    }
    eat(TokenKind.PP_SEMICOLON);
    return (node as ASTNode);
};

function parse_export(): ASTNode {
    expect(TokenKind.KW_EXPORT);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_EXPORT,
        init: undefined
    };
    if (peek(TokenKind.KW_LET) || peek(TokenKind.KW_CONST) || peek(TokenKind.KW_FUNCTION)) {
        node.init = parse_statement();
    }
    return (node as ASTNode);
};

function parse_while_statement(): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_WHILE,
        condition: undefined,
    };
    expect(TokenKind.KW_WHILE);
    node.condition = parse_expression();
    // braced body
    if (eat(TokenKind.PP_LBRACE)) {
        push_scope(node);
        node.body = parse_statement_list();
        pop_scope();
        expect(TokenKind.PP_RBRACE);
    // short body
    }
    // else {
    //     node.body = parse_expression();
    // }
    return (node as ASTNode);
};

function parse_if_statement(): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_IF,
        condition: undefined,
        alternate: undefined,
        // consequent: undefined
    };
    // else
    if (!eat(TokenKind.KW_IF)) {
        push_scope(node);
        node.consequent = parse_if_body();
        pop_scope();
        return (node as ASTNode);
    }
    expect(TokenKind.PP_LPAREN);
    node.condition = parse_expression();
    expect(TokenKind.PP_RPAREN);
    push_scope(node);
    node.consequent = parse_if_body();
    pop_scope();
    if (eat(TokenKind.KW_ELSE)) {
        node.alternate = parse_if_statement();
    }
    return (node as ASTNode);
};

function parse_if_body(): ASTNode[] {
    let node: ASTNode[];
    // braced if
    expect(TokenKind.PP_LBRACE);
    // if (eat(TokenKind.PP_LBRACE)) {
        node = parse_statement_list();
        expect(TokenKind.PP_RBRACE);
    // short if
    // } else {
    //     node = [];
    //     node.push(parse_expression());
    //     eat(TokenKind.PP_SEMICOLON);
    // }
    return (node);
};

function parse_return_statement(): ASTNode {
    expect(TokenKind.KW_RETURN);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_RETURN,
        argument: parse_expression()
    };
    return (node as ASTNode);
};

function parse_function_declaration(): ASTNode {
    expect(TokenKind.KW_FUNCTION);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_FUNCTION,
        // id: undefined,
        // parameter: undefined,
        // body: undefined
    };
    if (peek(TokenKind.TT_IDENTIFIER)) {
        node.id = current.value;
        scope.register(node.id, node as ASTNode);
        next();
    }
    node.parameter = parse_function_parameters();
    if (eat(TokenKind.PP_LBRACE)) {
        push_scope(node);
        node.body = parse_statement_list();
        pop_scope();
        expect(TokenKind.PP_RBRACE);
    }
    return (node as ASTNode);
};

function parse_function_parameters(): FunctionNode["parameter"] {
    let params = [];
    expect(TokenKind.PP_LPAREN);
    while (true) {
        if (peek(TokenKind.PP_RPAREN)) break;
        expect_identifier();
        if (current.value == "inout") {
            next();
            expect_identifier();
            // current.is_inout = true;
            params.push(current);
        } else {
            // current.is_inout = false;
            params.push(current);
        }
        let param = params[params.length - 1];
        param.is_parameter = true;
        scope.register(param.value, param);
        next();
        if (!eat(TokenKind.PP_COMMA)) break;
    };
    expect(TokenKind.PP_RPAREN);
    return (params);
};

function parse_enum_declaration(): ASTNode {
    expect(TokenKind.KW_ENUM);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_ENUM,
        // name: undefined,
        // body: undefined
    };
    expect_identifier();
    node.name = current.value;
    scope.register(node.name, node as ASTNode);
    next();
    expect(TokenKind.PP_LBRACE);
    node.body = parse_enum_body();
    expect(TokenKind.PP_RBRACE);
    return (node as ASTNode);
};

function parse_enum_expression(): ASTNode {
    let name = undefined;
    let member = undefined;
    let isShorty = eat(TokenKind.PP_DOT);
    // shorty, try to auto resolve enum
    if (isShorty) {
        expect_identifier();
        let nameToResolve = current.value;
        let cscope = scope;
        while (cscope != undefined) {
            let sym = cscope.symbols;
            let keys = Object.keys(sym);
            let kk = 0;
            while (kk < keys.length) {
                let key = keys[kk];
                let item = sym[key];
                if (item.kind == TokenKind.NN_ENUM) {
                    let jj = 0;
                    while (jj < item.body.length) {
                        let child = item.body[jj];
                        if (child.name == nameToResolve) {
                            name = item.name;
                            member = nameToResolve;
                            // break all loops
                            cscope = { parent: undefined }; kk = keys.length + 1; break;
                        }
                        jj++;
                    }
                }
                kk++;
            };
            cscope = cscope.parent;
        };
    } else {
        name = current.value;
        expect(TokenKind.PP_DOT);
    }
    expect_identifier();
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_ENUM_EXPRESSION,
        value: undefined
    };
    // unfold enum
    let resolve = scope.resolve(name as string);
    if (resolve && resolve.kind == TokenKind.NN_ENUM) {
        let ii = 0;
        let body = resolve.body;
        while (ii < body.length) {
            let child = body[ii];
            if (child.name == member) {
                node.value = child.init;
                break;
            }
            ii++;
        };
    }
    next();
    return (node as ASTNode);
};

function parse_enum_body(): EnumItemNode[] {
  let keys: EnumItemNode[] = [];
  let idx = 0;
  while (peek(TokenKind.TT_IDENTIFIER)) {
        let node: Partial<ASTNode> = {
            kind: TokenKind.NN_ENUM_ITEM,
            name: current.value,
            init: undefined
        };
        next();
        if (eat(TokenKind.OP_ASS)) {
            if (!is_literal(current)) {
                console.error("Enum key " + node.name + " can only have numeric value");
            } else {
                node.init = parse_literal();
                idx = node.init.value;
            }
        } else {
            node.init = {value:idx++};
        }
        scope.register(node.name, node);
        keys.push(node);
        if (!eat(TokenKind.PP_COMMA)) break;
    };
    return (keys);
};

function parse_variable_declaration(kind: TokenKind): ASTNode {
    next();
    expect_identifier();
    let node = {
        kind: kind,
        id: current.value,
        init: undefined
    };
    next();
    scope.register(node.id, node);
    expect(TokenKind.OP_ASS);
    node.init = parse_expression();
    return (node as ASTNode);
};

function parse_member_expression(parent: ASTNode): ASTNode {
    expect(TokenKind.PP_DOT);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_MEMBER_EXPRESSION,
        parent: parent,
        member: parse_expression()
    };
    if (node.parent.kind == TokenKind.NN_LITERAL && node.member.kind == TokenKind.NN_LITERAL) {
        let resolve = scope.resolve(node.parent.value);
        if (resolve && resolve.kind == TokenKind.NN_ENUM) {
            let ii = 0;
            while (ii < resolve.body.length) {
                let child = resolve.body[ii];
                if (node.member.value == child.name) {
                    node = {
                        kind: TokenKind.NN_ENUM_EXPRESSION,
                        value: child.init
                    };
                    break;
                }
                ii++;
            };
        }
    }
    return (node as ASTNode);
};

function parse_computed_member_expression(parent: ASTNode): ASTNode {
    expect(TokenKind.PP_LBRACK);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_COMPUTED_MEMBER_EXPRESSION,
        parent: parent,
        member: parse_expression()
    };
    expect(TokenKind.PP_RBRACK);
    return (node as ASTNode);
};

function parse_call_expression(id): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_CALL_EXPRESSION,
        callee: id,
        parameter: parse_call_parameters()
    };
    let resolve = scope.resolve(id.value);
    if (resolve && resolve.kind == TokenKind.NN_FUNCTION) {
        let params = resolve.parameter;
        let idx = 0;
        params.map(function(item) {
            let param = node.parameter[idx];
            let loc = id.value + "::" + param.value;
            if (item.isInout) {
                if (param.kind != .NN_LITERAL) {
                    console.error("Function " + loc + " is inout and only accepts literals");
                } else {
                    // now try to trace the variable declaration location as a later pointer
                    let resolve = scope.resolve(param.value);
                    if (resolve?.kind != TokenKind.NN_LET) {
                        console.error("Passing by reference in " + loc + " only accepts variables right now");
                    } else {
                        // trace as later reference
                        if (!resolve.isLaterReference) {
                            resolve.isLaterReference = true;
                        }
                    }
                }
            }
            idx++;
        });
    }
    return (node as ASTNode);
};

function parse_call_parameters() {
    let params = [];
    expect(TokenKind.PP_LPAREN);
    while (true) {
        if (peek(TokenKind.PP_RPAREN)) break;
        let expr = parse_expression();
        params.push(expr);
        if (!eat(TokenKind.PP_COMMA)) break;
    };
    expect(TokenKind.PP_RPAREN);
    return (params);
};

function parse_break(): ASTNode {
    expect(TokenKind.KW_BREAK);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_BREAK
    };
    return (node as ASTNode);
};

function parse_continue(): ASTNode {
    expect(TokenKind.KW_CONTINUE);
    let node = {
        kind: TokenKind.NN_CONTINUE
    };
    return (node as ASTNode);
};

function parse_object_expression(): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_OBJECT_EXPRESSION,
        properties: []
    };
    expect(TokenKind.PP_LBRACE);
    while (true) {
        if (peek(TokenKind.PP_RBRACE)) break;
            let property = {
                kind: TokenKind.NN_OBJECT_PROPERTY,
                id: parse_literal(),
                value: undefined
            };
        expect(TokenKind.PP_COLON);
        property.value = parse_expression();
        node.properties.push(property);
        if (!eat(TokenKind.PP_COMMA)) break;
    };
    expect(TokenKind.PP_RBRACE);
    return (node as ASTNode);
};

function parse_unary_prefix_expression(): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_UNARY_PREFIX_EXPRESSION,
        operator: current.value,
        value: undefined
    };
    next();
    node.value = parse_literal();
    return (node as ASTNode);
};

function parse_unary_postfix_expression(left: ASTNode): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_UNARY_POSTFIX_EXPRESSION,
        operator: current.value,
        value: left
    };
    next();
    return (node as ASTNode);
};

function parse_binary_expression(left: ASTNode): ASTNode {
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_BINARY_EXPRESSION,
        left: left,
        right: undefined,
        operator: current.value
    };
    next();
    node.right = parse_expression();
    return (node as ASTNode);
};

function parse_infix(left: ASTNode) {
    if (is_binary_operator(current)) {
        return (parse_binary_expression(left));
    }
    if (is_unary_postfix_operator(current)) {
        return (parse_unary_postfix_expression(left));
    }
    if (peek(TokenKind.PP_LPAREN)) {
        return (parse_call_expression(left));
    }
    if (peek(TokenKind.PP_DOT)) {
        return (parse_member_expression(left));
    }
    if (peek(TokenKind.PP_LBRACK)) {
        return (parse_computed_member_expression(left));
    }
    return (left);
};

function parse_prefix(): ASTNode {
    if (is_literal(current)) {
        return (parse_literal());
    }
    if (peek(TokenKind.PP_LBRACE)) {
        return (parse_object_expression());
    }
    if (peek(TokenKind.PP_LBRACK)) {
        return (parse_array_expression());
    }
    if (eat(TokenKind.PP_LPAREN)) {
        let node = parse_expression();
        expect(TokenKind.PP_RPAREN);
        return (node as ASTNode);
    }
    if (is_unary_prefix_operator(current)) {
        return (parse_unary_prefix_expression());
    }
    return (parse_statement());
};

function parse_array_expression(): ASTNode {
    expect(TokenKind.PP_LBRACK);
    let node: Partial<ASTNode> = {
        kind: TokenKind.NN_ARRAY_EXPRESSION,
        elements: []
    };
    while (true) {
        if (peek(TokenKind.PP_RBRACK)) break;
        let element = {
            kind: TokenKind.NN_ARRAY_ELEMENT,
            value: parse_expression()
        };
        node.elements.push(element);
        if (!eat(TokenKind.PP_COMMA)) break;
    };
    expect(TokenKind.PP_RBRACK);
    return (node as ASTNode);
};

function parse_expression(): ASTNode {
    if (peek(TokenKind.KW_BREAK)) {
        return (parse_break());
    }
    if (peek(TokenKind.KW_CONTINUE)) {
        return (parse_continue());
    }
    if (peek(TokenKind.PP_DOT)) {
        return (parse_enum_expression());
    }
    let node = parse_prefix();
    while (true) {
        if (!current) break;
        let expr = parse_infix(node);
        if (expr == undefined || expr == node) break;
        node = expr;
    };
    return (node as ASTNode);
};

function parse_literal() {
    if (peek(TokenKind.TT_STRING)) {
        return (parse_string_literal());
    }
    let node = {
        kind: TokenKind.NN_LITERAL,
        type: current.kind,
        value: current.value
    };
    next();
    return (node as ASTNode);
};

function parse_string_literal() {
    let node = {
        kind: TokenKind.NN_STRING_LITERAL,
        type: current.kind,
        value: current.value,
        isChar: current.is_char
    };
    next();
    return (node as ASTNode);
};

// GENERATOR
let out = "";
function write(str: string) {
    out = out + str;
};

function generate(node: Partial<ASTNode>): string {
  out = "";
  generate_body(node.body);
  return (out);
};

function generate_body(body) {
    let ii = 0;
    while (ii < body.length) {
        generate_node(body[ii]);
        ii++;
        write(";");
    };
};

function generate_node(node: Partial<ASTNode>) {
  let kind = node.kind;
  if (kind == TokenKind.NN_FUNCTION) {
        write("function ");
        if (node.id) write(node.id);
        write("(");
        let ii = 0;
        push_scope(node);
        while (ii < node.parameter.length) {
            write(node.parameter[ii].value);
            if (ii + 1 < node.parameter.length) {
                write(", ");
            }
            ii++;
        };
        write(")");
        write(" { ");
        generate_body(node.body);
        write(" } ");
        pop_scope();
    } else if (kind == TokenKind.NN_LET) {
        let isLaterReference = node.isLaterReference;
        write("let ");
        write(node.id);
        write(" = ");
        if (isLaterReference) {
            write("{");
            write("$iov:");
        }
        generate_node(node.init);
        if (isLaterReference) {
            write("}");
        }
    } else if (kind == TokenKind.NN_CONST) {
        write("const ");
        write(node.id);
        write(" = ");
        generate_node(node.init);
    }
    else if (kind == TokenKind.NN_IF) {
        if (node.condition) {
            write("if (");
            generate_node(node.condition);
            write(")");
        }
        write(" { ");
        push_scope(node.consequent);
        generate_body(node.consequent);
        pop_scope();
        write(" } ");
        if (node.alternate) {
            write("else ");
            push_scope(node.alternate);
            generate_node(node.alternate);
            pop_scope();
        }
    } else if (kind == TokenKind.NN_RETURN) {
        write("return (");
        generate_node(node.argument);
        write(")");
    } else if (kind == TokenKind.NN_WHILE) {
        write("while ");
        write("(");
        generate_node(node.condition);
        write(")");
        push_scope(node);
        write(" {");
        generate_body(node.body);
        write(" } ");
        pop_scope();
    } else if (kind == TokenKind.NN_BREAK) {
        write("break");
        write("");
    } else if (kind == TokenKind.NN_CONTINUE) {
        write("continue");
        write("");
    } else if (kind == TokenKind.NN_CALL_EXPRESSION) {
        let callee = node.callee;
        let resolve = scope.resolve(callee.value);
        generate_node(callee);
        write("(");
        let ii = 0;
        while (ii < node.parameter.length) {
            // pass identifier by reference
            if (resolve && resolve.parameter[ii].isInout) {
                write(node.parameter[ii].value);
            } else {
                generate_node(node.parameter[ii]);
            }
            if (ii + 1 < node.parameter.length) {
                write(", ");
            }
            ii++;
        };
        write(")");
    } else if (kind == TokenKind.NN_BINARY_EXPRESSION) {
        generate_node(node.left);
        if (node.operator == "==") {
            write(" === ");
        } else if (node.operator == "!=") {
            write(" !== ");
        } else {
            write(node.operator);
        }
        generate_node(node.right);
    } else if (kind == TokenKind.NN_MEMBER_EXPRESSION) {
        generate_node(node.parent);
        write(".");
        generate_node(node.member);
    }  else if (kind == TokenKind.NN_COMPUTED_MEMBER_EXPRESSION) {
        generate_node(node.parent);
        write("[");
        generate_node(node.member);
        write("]");
    } else if (kind == TokenKind.NN_UNARY_PREFIX_EXPRESSION) {
        if (node.operator == "new") {
            write(node.operator);
            write(" ");
        } else write(node.operator);
        generate_node(node.value);
    } else if (kind == TokenKind.NN_UNARY_POSTFIX_EXPRESSION) {
        generate_node(node.value);
        write(node.operator);
    } else if (kind == TokenKind.NN_OBJECT_EXPRESSION) {
        write("{");
        let ii = 0;
        while (ii < node.properties.length) {
            let property = node.properties[ii];
            generate_node(property.id);
            write(": ");
            generate_node(property.value);
            if (ii + 1 < node.properties.length) {
                write(", ");
            }
            ii++;
        };
        write(" }");
    } else if (kind == TokenKind.NN_ARRAY_EXPRESSION) {
        write("[");
        let ii = 0;
        while (ii < node.elements.length) {
            let element = node.elements[ii];
            generate_node(element.value);
            if (ii + 1 < node.elements.length) {
                write(", ");
            }
            ii++;
        };
        write("]");
    } else if (kind == TokenKind.NN_LITERAL) {
        let resolve = scope.resolve(node.value);
        write(node.value);
        if (resolve) {
            if (resolve.isLaterReference) {
                write(".$iov");
            } else if (resolve.isParameter && resolve.isInout) {
                write(".$iov");
            }
        }
    } else if (kind == TokenKind.NN_STRING_LITERAL) {
        let isChar = node.isChar;
        if (isChar) write('"');
        else write("'");
        write(node.value);
        if (isChar) write('"');
        else write("'");
    } else if (kind == TokenKind.NN_EXPORT) {
        let init = node.init;
        write("module.exports.");
        write(init.id);
        write(" = ");
        if (init.kind == .NN_FUNCTION) {
            generate_node(init);
        } else if (init.kind == .NN_LET || init.kind == .NN_CONST) {
            generate_node(init.init);
        } else {
            console.error("Cannot export node kind " + init.kind + "!");
        }
    } else if (kind == TokenKind.NN_ENUM) {
        let name = node.name;
        let body = node.body;
        write("var ");
        write(name);
        write(";");
        write("(function(");
        write(name);
        write(") {");
        // body
        let ii = 0;
        while (ii < body.length) {
            let child = body[ii];
            write(name);
            write("[");
            write(name);
            write("[");
            write("'" + child.name + "'");
            write("]");
            write(" = ");
            write(child.init.value);
            write("] = ");
            write("'" + child.name + "'");
            write(";");
            ii++;
        };
        write("})(");
        write(name);
        write(" || (");
        write(name);
        write(" = {})");
        write(")");
    } else if (kind == TokenKind.NN_ENUM_EXPRESSION) {
        write(node.value.value);
    } else {
        console.error("Unknown node kind " + node.kind + "!");
    }
};

export function compile(str: string) {
    let tokens = scan(str);
    let ast = parse(tokens);
    return (generate(ast));
};
