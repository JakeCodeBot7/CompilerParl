from Lexer import TokenType, Lexer, Token
from AST import *

class Parser:
    def __init__(self, source_code):
        self.lexer = Lexer(source_code)
        self.current_token = None
        self.tokens = self.lexer.tokens  # Pre-tokenized list
        self.current_index = 0
        self.advance()

    def advance(self):
        # Move to next non-whitespace/comment token
        while True:
            if self.current_index < len(self.tokens):
                self.current_token = self.tokens[self.current_index]
                self.current_index += 1
            else:
                self.current_token = Token(TokenType.EOF, "EOF")
            if self.current_token.type not in {
                TokenType.WHITESPACE, TokenType.COMMENT, TokenType.NEWLINE
            }:
                break

    def peek(self):
        # Look ahead to next non-skippable token
        index = self.current_index
        while index < len(self.tokens):
            tok = self.tokens[index]
            if tok.type not in {TokenType.WHITESPACE, TokenType.NEWLINE, TokenType.COMMENT}:
                return tok
            index += 1
        return Token(TokenType.EOF, "EOF")

    def match(self, expected_type):
        if self.current_token.type == expected_type:
            self.advance()
        else:
            raise SyntaxError(f"Expected {expected_type}, found {self.current_token.type}")

    def parse(self):
        return self.parse_program()

    def parse_program(self):
        program_node = ASTProgramNode()
        while self.current_token.type != TokenType.EOF:
            stmt = self.parse_statement()
            program_node.add_statement(stmt)
        return program_node

    def parse_statement(self):
        # Dispatch based on current token type
        if self.current_token.type == TokenType.PRINT:
            return self.parse_print_statement()
        elif self.current_token.type == TokenType.DELAY:
            return self.parse_delay_statement()
        elif self.current_token.type == TokenType.WRITE:
            return self.parse_write_statement()
        elif self.current_token.type == TokenType.WRITE_BOX:
            return self.parse_write_box_statement()
        elif self.current_token.type == TokenType.CLEAR:
            return self.parse_clear_statement()
        elif self.current_token.type == TokenType.fun:
            return self.parse_function()
        elif self.current_token.type == TokenType.let:
            return self.parse_variable_declaration()
        elif self.current_token.type == TokenType.if_:
            return self.parse_if()
        elif self.current_token.type == TokenType.while_:
            return self.parse_while()
        elif self.current_token.type == TokenType.for_:
            return self.parse_for_statement()
        elif self.current_token.type == TokenType.return_:
            return self.parse_return()
        elif self.current_token.type == TokenType.IDENTIFIER:
            next_tok = self.peek()
            if next_tok.type == TokenType.OP_ASSIGN:
                return self.parse_assignment()
            else:
                return self.parse_expression_statement()
        elif self.is_start_of_expression(self.current_token.type):
            return self.parse_expression_statement()
        else:
            raise SyntaxError(f"Unexpected token in statement: {self.current_token}")

    # ----------------------------------
    # Expression/starters + helpers
    # ----------------------------------

    def is_start_of_expression(self, token_type):
        return token_type in {
            TokenType.IDENTIFIER,
            TokenType.INTEGER_LITERAL,
            TokenType.FLOAT_LITERAL,
            TokenType.COLOUR_LITERAL,
            TokenType.BOOLEAN_LITERAL,
            TokenType.true, TokenType.false,
            TokenType.LPAREN,
            TokenType.PRINT, TokenType.DELAY, TokenType.WRITE,
            TokenType.WRITE_BOX, TokenType.CLEAR, TokenType.RANDOM_INT,
            TokenType.READ, TokenType.WIDTH, TokenType.HEIGHT
        }

    def parse_variable_declaration_no_semicolon(self):
        # Used inside for-loops
        self.match(TokenType.let)
        name = self.current_token.value
        self.match(TokenType.IDENTIFIER)
        self.match(TokenType.COLON)
        var_type = self.current_token.value
        self.advance()

        expr = None
        if self.current_token.type == TokenType.OP_ASSIGN:
            self.match(TokenType.OP_ASSIGN)
            expr = self.parse_expression()

        return ASTVariableDeclarationNode(var_type, name, expr)

    def parse_assignment_no_semicolon(self):
        name = self.current_token.value
        self.match(TokenType.IDENTIFIER)
        self.match(TokenType.OP_ASSIGN)
        expr = self.parse_expression()
        return ASTAssignmentNode(name, expr)

    # ----------------------------------
    # For Loop
    # ----------------------------------

    def parse_for_statement(self):
        self.match(TokenType.for_)
        self.match(TokenType.LPAREN)

        # init part (declaration or assignment)
        init_stmt = None
        if self.current_token.type == TokenType.let:
            init_stmt = self.parse_variable_declaration_no_semicolon()
        elif self.current_token.type != TokenType.SEMICOLON:
            init_stmt = self.parse_assignment_no_semicolon()
        self.match(TokenType.SEMICOLON)

        # condition part
        condition_expr = None
        if self.current_token.type != TokenType.SEMICOLON:
            condition_expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)

        # increment part
        increment_stmt = None
        if self.current_token.type != TokenType.RPAREN:
            increment_stmt = self.parse_assignment_no_semicolon()
        self.match(TokenType.RPAREN)

        body_stmt = self.parse_block()
        return ASTForNode(init_stmt, condition_expr, increment_stmt, body_stmt)

    def parse_expression_statement(self):
        expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTExpressionStatement(expr)

    # ----------------------------------
    # Built-in Statements
    # ----------------------------------

    def parse_print_statement(self):
        self.match(TokenType.PRINT)
        expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTPrintNode(expr)

    def parse_delay_statement(self):
        self.match(TokenType.DELAY)
        expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTDelayNode(expr)

    def parse_write_statement(self):
        self.match(TokenType.WRITE)
        x_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        y_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        color_expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTWriteNode(x_expr, y_expr, color_expr)

    def parse_write_box_statement(self):
        self.match(TokenType.WRITE_BOX)
        x_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        y_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        w_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        h_expr = self.parse_expression()
        self.match(TokenType.COMMA)
        color_expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTWriteBoxNode(x_expr, y_expr, w_expr, h_expr, color_expr)

    def parse_clear_statement(self):
        self.match(TokenType.CLEAR)
        color_expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTClearNode(color_expr)

    # ----------------------------------
    # Function Declaration
    # ----------------------------------

    def parse_function(self):
        self.match(TokenType.fun)
        name = self.current_token.value
        self.match(TokenType.IDENTIFIER)
        self.match(TokenType.LPAREN)

        # Parse parameters
        params = []
        if self.current_token.type != TokenType.RPAREN:
            while True:
                param_name = self.current_token.value
                self.match(TokenType.IDENTIFIER)
                self.match(TokenType.COLON)
                base_type = self.current_token.value
                self.advance()

                # Optional array type
                if self.current_token.type == TokenType.LBRACKET:
                    self.match(TokenType.LBRACKET)
                    if self.current_token.type != TokenType.INTEGER_LITERAL:
                        raise SyntaxError("Expected array size")
                    size = self.current_token.value
                    self.match(TokenType.INTEGER_LITERAL)
                    self.match(TokenType.RBRACKET)
                    param_type = f"{base_type}[{size}]"
                else:
                    param_type = base_type

                params.append((param_type, param_name))

                if self.current_token.type == TokenType.COMMA:
                    self.match(TokenType.COMMA)
                else:
                    break

        self.match(TokenType.RPAREN)
        self.match(TokenType.ARROW)
        return_type = self.current_token.value
        self.advance()

        body = self.parse_block()
        return ASTFunctionNode(name, params, return_type, body)

    # ----------------------------------
    # Variable Declaration
    # ----------------------------------

    def parse_variable_declaration(self):
        self.match(TokenType.let)
        name = self.current_token.value
        self.match(TokenType.IDENTIFIER)
        self.match(TokenType.COLON)
        var_type = self.current_token.value
        self.advance()

        if self.current_token.type == TokenType.LBRACKET:
            return self.parse_array_declaration(name, var_type)

        expr = None
        if self.current_token.type == TokenType.OP_ASSIGN:
            self.match(TokenType.OP_ASSIGN)
            expr = self.parse_expression()

        self.match(TokenType.SEMICOLON)
        return ASTVariableDeclarationNode(var_type, name, expr)

    def parse_array_declaration(self, name, element_type):
        self.match(TokenType.LBRACKET)

        size_expr = None
        if self.current_token.type == TokenType.INTEGER_LITERAL:
            size_expr = ASTLiteralNode(self.current_token.value, "int")
            self.match(TokenType.INTEGER_LITERAL)

        self.match(TokenType.RBRACKET)
        self.match(TokenType.OP_ASSIGN)
        self.match(TokenType.LBRACKET)

        # Array initializer
        values = []
        while True:
            lit_token = self.current_token
            if lit_token.type not in {
                TokenType.INTEGER_LITERAL, TokenType.FLOAT_LITERAL,
                TokenType.COLOUR_LITERAL, TokenType.BOOLEAN_LITERAL
            }:
                raise SyntaxError(f"Expected literal, got {lit_token}")

            lit_type = {
                TokenType.INTEGER_LITERAL: "int",
                TokenType.FLOAT_LITERAL: "float",
                TokenType.COLOUR_LITERAL: "colour",
                TokenType.BOOLEAN_LITERAL: "bool",
                TokenType.true: "bool",
                TokenType.false: "bool"
            }.get(lit_token.type, "unknown")

            values.append(ASTLiteralNode(lit_token.value, lit_type))
            self.advance()

            if self.current_token.type == TokenType.COMMA:
                self.match(TokenType.COMMA)
            else:
                break

        self.match(TokenType.RBRACKET)
        self.match(TokenType.SEMICOLON)
        return ASTArrayDeclarationNode(element_type, name, size_expr, values)

    # ----------------------------------
    # Assignment
    # ----------------------------------

    def parse_assignment(self):
        name = self.current_token.value
        self.match(TokenType.IDENTIFIER)

        if self.current_token.type == TokenType.LBRACKET:
            self.match(TokenType.LBRACKET)
            index_expr = self.parse_expression()
            self.match(TokenType.RBRACKET)
            self.match(TokenType.OP_ASSIGN)
            value_expr = self.parse_expression()
            self.match(TokenType.SEMICOLON)
            return ASTArrayAssignmentNode(name, index_expr, value_expr)

        self.match(TokenType.OP_ASSIGN)
        expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTAssignmentNode(name, expr)

    # ----------------------------------
    # Conditionals
    # ----------------------------------

    def parse_if(self):
        self.match(TokenType.if_)
        self.match(TokenType.LPAREN)
        condition = self.parse_expression()
        self.match(TokenType.RPAREN)
        then_block = self.parse_block()
        else_block = None
        if self.current_token.type == TokenType.else_:
            self.match(TokenType.else_)
            else_block = self.parse_block()
        return ASTIfNode(condition, then_block, else_block)

    def parse_while(self):
        self.match(TokenType.while_)
        self.match(TokenType.LPAREN)
        condition = self.parse_expression()
        self.match(TokenType.RPAREN)
        body = self.parse_block()
        return ASTWhileNode(condition, body)

    def parse_return(self):
        self.match(TokenType.return_)
        expr = self.parse_expression()
        self.match(TokenType.SEMICOLON)
        return ASTReturnNode(expr)

    def parse_block(self):
        self.match(TokenType.LBRACE)
        block = ASTBlockNode()
        while self.current_token.type != TokenType.RBRACE:
            block.add_statement(self.parse_statement())
        self.match(TokenType.RBRACE)
        return block

    # ----------------------------------
    # Expression Chain
    # ----------------------------------

    def parse_expression(self):
        expr = self.parse_simple_expr()

        while self.current_token.type in {
            TokenType.OP_LT, TokenType.OP_GT, TokenType.OP_LE, TokenType.OP_GE,
            TokenType.OP_EQ, TokenType.OP_NEQ
        }:
            op = self.current_token.value
            self.advance()
            right = self.parse_simple_expr()
            expr = ASTBinaryOpNode(expr, op, right)

        while self.current_token.type == TokenType.OP_CAST:
            self.match(TokenType.OP_CAST)
            target_type = self.current_token.value
            self.advance()
            expr = ASTCastNode(expr, target_type)

        return expr

    def parse_simple_expr(self):
        expr = self.parse_term()
        while self.current_token.type in {
            TokenType.OP_ADD, TokenType.OP_SUB, TokenType.OP_OR
        }:
            op = self.current_token.value
            self.advance()
            right = self.parse_term()
            expr = ASTBinaryOpNode(expr, op, right)
        return expr

    def parse_term(self):
        expr = self.parse_factor()
        while self.current_token.type in {
            TokenType.OP_MUL, TokenType.OP_DIV, TokenType.OP_AND
        }:
            op = self.current_token.value
            self.advance()
            right = self.parse_factor()
            expr = ASTBinaryOpNode(expr, op, right)
        return expr

    def parse_factor(self):
        return self.parse_unary()

    def parse_unary(self):
        if self.current_token.type in {TokenType.OP_NOT, TokenType.OP_SUB}:
            op = self.current_token.value
            self.advance()
            operand = self.parse_unary()
            return ASTUnaryOpNode(op, operand)
        else:
            return self.parse_primary()

    def parse_argument_list(self):
        args = []
        if self.current_token.type != TokenType.RPAREN:
            while True:
                args.append(self.parse_expression())
                if self.current_token.type == TokenType.COMMA:
                    self.match(TokenType.COMMA)
                else:
                    break
        self.match(TokenType.RPAREN)
        return args

    def parse_primary(self):
        if self.current_token.type == TokenType.OP_ASSIGN:
            raise SyntaxError("Assignment is not a valid expression.")

        if self.current_token.type == TokenType.LPAREN:
            self.match(TokenType.LPAREN)
            expr = self.parse_expression()
            self.match(TokenType.RPAREN)
            return expr

        elif self.current_token.type in {
            TokenType.IDENTIFIER, TokenType.PRINT, TokenType.DELAY,
            TokenType.WRITE, TokenType.WRITE_BOX, TokenType.CLEAR,
            TokenType.RANDOM_INT, TokenType.READ, TokenType.WIDTH,
            TokenType.HEIGHT
        }:
            name = self.current_token.value
            self.advance()
            if self.current_token.type == TokenType.LPAREN:
                self.match(TokenType.LPAREN)
                args = self.parse_argument_list()
                return ASTFunctionCallNode(name, args)
            elif self.current_token.type == TokenType.LBRACKET:
                self.match(TokenType.LBRACKET)
                index_expr = self.parse_expression()
                self.match(TokenType.RBRACKET)
                return ASTArrayAccessNode(name, index_expr)
            return ASTIdentifierNode(name)

        elif self.current_token.type == TokenType.INTEGER_LITERAL:
            val = self.current_token.value
            self.match(TokenType.INTEGER_LITERAL)
            return ASTLiteralNode(val, "int")

        elif self.current_token.type == TokenType.FLOAT_LITERAL:
            val = self.current_token.value
            self.match(TokenType.FLOAT_LITERAL)
            return ASTLiteralNode(val, "float")

        elif self.current_token.type == TokenType.COLOUR_LITERAL:
            val = self.current_token.value
            self.match(TokenType.COLOUR_LITERAL)
            return ASTLiteralNode(val, "colour")

        elif self.current_token.type in {TokenType.true, TokenType.false, TokenType.BOOLEAN_LITERAL}:
            val = self.current_token.value
            self.advance()
            return ASTLiteralNode(val, "bool")

        else:
            raise SyntaxError(f"Unexpected primary expression: {self.current_token}")
