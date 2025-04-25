class ASTNode:
    def accept(self, visitor):
        raise NotImplementedError()

class ASTStatementNode(ASTNode):
    pass

class ASTExpressionNode(ASTNode):
    pass

#Node Classses
class ASTLiteralNode(ASTExpressionNode):
    def __init__(self, value, literal_type="unknown"):
        self.value = value
        self.literal_type = literal_type

    def accept(self, visitor):
        return visitor.visit_literal_node(self)

class ASTIdentifierNode(ASTExpressionNode):
    def __init__(self, name):
        self.name = name

    def accept(self, visitor):
        return visitor.visit_identifier_node(self)

class ASTUnaryOpNode(ASTExpressionNode):
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def accept(self, visitor):
        return visitor.visit_unary_op_node(self)

class ASTBinaryOpNode(ASTExpressionNode):
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def accept(self, visitor):
        return visitor.visit_binary_op_node(self)
    
class ASTForNode(ASTStatementNode):
    def __init__(self, init_stmt, condition_expr, increment_stmt, body):
        self.init_stmt = init_stmt
        self.condition_expr = condition_expr
        self.increment_stmt = increment_stmt
        self.body = body

    def accept(self, visitor):
        return visitor.visit_for_node(self)

class ASTCastNode(ASTExpressionNode):
    def __init__(self, expression, target_type):
        self.expression = expression
        self.target_type = target_type

    def accept(self, visitor):
        return visitor.visit_cast_node(self)

class ASTProgramNode(ASTNode):
    def __init__(self):
        self.statements = []

    def add_statement(self, stmt):
        self.statements.append(stmt)

    def accept(self, visitor):
        return visitor.visit_program_node(self)

class ASTFunctionNode(ASTStatementNode):
    def __init__(self, name, params, return_type, body):
        self.name = name
        self.params = params
        self.return_type = return_type
        self.body = body

    def accept(self, visitor):
        return visitor.visit_function_node(self)
    
class ASTFunctionCallNode(ASTExpressionNode):
        def __init__(self, name, arguments):
            self.name = name
            self.arguments = arguments

        def accept(self, visitor):
            return visitor.visit_function_call_node(self)

class ASTVariableDeclarationNode(ASTStatementNode):
    def __init__(self, var_type, name, expr=None):
        self.var_type = var_type
        self.name = name
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_variable_declaration_node(self)
    
class ASTAssignmentNode(ASTStatementNode):
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_assignment_node(self)


class ASTIfNode(ASTStatementNode):
    def __init__(self, condition, then_block, else_block=None):
        self.condition = condition
        self.then_block = then_block
        self.else_block = else_block

    def accept(self, visitor):
        return visitor.visit_if_node(self)

class ASTWhileNode(ASTStatementNode):
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body

    def accept(self, visitor):
        return visitor.visit_while_node(self)

class ASTReturnNode(ASTStatementNode):
    def __init__(self, expr):
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_return_node(self)

class ASTBlockNode(ASTStatementNode):
    def __init__(self):
        self.statements = []

    def add_statement(self, stmt):
        self.statements.append(stmt)

    def accept(self, visitor):
        return visitor.visit_block_node(self)
    
class ASTPrintNode(ASTStatementNode):
    def __init__(self, expr):
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_print_node(self)

class ASTDelayNode(ASTStatementNode):
    def __init__(self, expr):
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_delay_node(self)

class ASTWriteNode(ASTStatementNode):
    def __init__(self, x_expr, y_expr, color_expr):
        self.x_expr = x_expr
        self.y_expr = y_expr
        self.color_expr = color_expr
    
    def accept(self, visitor):
        return visitor.visit_write_node(self)

class ASTWriteBoxNode(ASTStatementNode):
    def __init__(self, x_expr, y_expr, w_expr, h_expr, color_expr):
        self.x_expr = x_expr
        self.y_expr = y_expr
        self.w_expr = w_expr
        self.h_expr = h_expr
        self.color_expr = color_expr

    def accept(self, visitor):
        return visitor.visit_write_box_node(self)
    
class ASTExpressionStatement(ASTStatementNode):
    def __init__(self, expr):
        self.expr = expr

    def accept(self, visitor):
        return visitor.visit_expression_statement(self)


class ASTClearNode(ASTStatementNode):
    def __init__(self, color_expr):
        self.color_expr = color_expr

    def accept(self, visitor):
        return visitor.visit_clear_node(self)
    
class ASTArrayDeclarationNode(ASTStatementNode):
    def __init__(self, element_type, name, size_expr, values):
        self.element_type = element_type      
        self.name = name                    
        self.size_expr = size_expr            
        self.values = values                

    def accept(self, visitor):
        return visitor.visit_array_declaration_node(self)
    
class ASTArrayAccessNode(ASTExpressionNode):
    def __init__(self, array_name, index_expr):
        self.array_name = array_name  
        self.index_expr = index_expr  

    def accept(self, visitor):
        return visitor.visit_array_access_node(self)
    
class ASTArrayAssignmentNode(ASTStatementNode):
    def __init__(self, array_name, index_expr, value_expr):
        self.array_name = array_name
        self.index_expr = index_expr
        self.value_expr = value_expr

    def accept(self, visitor):
        return visitor.visit_array_assignment_node(self)




class ASTVisitor:
    # Program and function structure
    def visit_program_node(self, node): pass
    def visit_function_node(self, node): pass
    def visit_block_node(self, node): pass
    def visit_return_node(self, node): pass

    # Statements
    def visit_variable_declaration_node(self, node): pass
    def visit_assignment_node(self, node): pass
    def visit_expression_statement(self, node): pass
    def visit_if_node(self, node): pass
    def visit_for_node(self, node): pass
    def visit_while_node(self, node): pass
    def visit_print_node(self, node): pass
    def visit_delay_node(self, node): pass
    def visit_write_node(self, node): pass
    def visit_write_box_node(self, node): pass
    def visit_clear_node(self, node): pass

    # Expressions
    def visit_literal_node(self, node): pass
    def visit_identifier_node(self, node): pass
    def visit_binary_op_node(self, node): pass
    def visit_unary_op_node(self, node): pass
    def visit_cast_node(self, node): pass
    def visit_function_call_node(self, node): pass
    def visit_array_declaration_node(self, node): pass
    def visit_array_access_node(self, node): pass
    def visit_array_assignment_node(self, node): pass





class ASTPrinter(ASTVisitor):
    def __init__(self):
        self.indent = 0

    def print_indent(self, text):
        print("  " * self.indent + text)

    def visit_program_node(self, node):
        self.print_indent("Program:")
        self.indent += 1
        for stmt in node.statements:
            stmt.accept(self)
        self.indent -= 1

    def visit_function_node(self, node):
        self.print_indent(f"Function: {node.name} -> {node.return_type}")
        self.indent += 1
        node.body.accept(self)
        self.indent -= 1

    def visit_function_call_node(self, node):
        self.print_indent(f"FunctionCall: {node.name}")
        self.indent += 1
        for arg in node.arguments:
            arg.accept(self)
        self.indent -= 1

    def visit_expression_statement(self, node):
        self.print_indent("ExprStatement:")
        self.indent += 1
        node.expr.accept(self)
        self.indent -= 1

    def visit_block_node(self, node):
        self.print_indent("Block:")
        self.indent += 1
        for stmt in node.statements:
            stmt.accept(self)
        self.indent -= 1

    def visit_variable_declaration_node(self, node):
        self.print_indent(f"VarDecl: {node.var_type} {node.name}")
        if node.expr:
            self.print_indent("Expr:")
            self.indent += 1
            node.expr.accept(self)
            self.indent -= 1

    def visit_assignment_node(self, node):
        self.print_indent(f"Assign: {node.name}")
        self.indent += 1
        node.expr.accept(self)
        self.indent -= 1

    def visit_if_node(self, node):
        self.print_indent("If:")
        self.indent += 1
        node.condition.accept(self)
        self.indent -= 1
        self.print_indent("Then:")
        self.indent += 1
        node.then_block.accept(self)
        self.indent -= 1
        if node.else_block:
            self.print_indent("Else:")
            self.indent += 1
            node.else_block.accept(self)
            self.indent -= 1
    
    def visit_for_node(self, node):
        self.print_indent("For:")
        self.indent += 1

        if node.init_stmt:
            self.print_indent("Init:")
            self.indent += 1
            node.init_stmt.accept(self)
            self.indent -= 1

        if node.condition_expr:
            self.print_indent("Condition:")
            self.indent += 1
            node.condition_expr.accept(self)
            self.indent -= 1

        if node.increment_stmt:
            self.print_indent("Increment:")
            self.indent += 1
            node.increment_stmt.accept(self)
            self.indent -= 1

        self.print_indent("Body:")
        self.indent += 1
        node.body.accept(self)
        self.indent -= 2

    def visit_while_node(self, node):
        self.print_indent("While:")
        self.indent += 1
        node.condition.accept(self)
        node.body.accept(self)
        self.indent -= 1

    def visit_return_node(self, node):
        self.print_indent("Return:")
        self.indent += 1
        node.expr.accept(self)
        self.indent -= 1

    def visit_binary_op_node(self, node):
        self.print_indent(f"BinOp: {node.operator}")
        self.indent += 1
        node.left.accept(self)
        node.right.accept(self)
        self.indent -= 1

    def visit_unary_op_node(self, node):
        self.print_indent(f"UnaryOp: {node.operator}")
        self.indent += 1
        node.operand.accept(self)
        self.indent -= 1

    def visit_cast_node(self, node):
        self.print_indent(f"Cast to: {node.target_type}")
        self.indent += 1
        node.expression.accept(self)
        self.indent -= 1

    def visit_literal_node(self, node):
        self.print_indent(f"Literal ({node.literal_type}): {node.value}")

    def visit_identifier_node(self, node):
        self.print_indent(f"Identifier: {node.name}")

    def visit_print_node(self, node):
        self.print_indent("Print:")
        self.indent += 1
        node.expr.accept(self)
        self.indent -= 1

    def visit_delay_node(self, node):
        self.print_indent("Delay:")
        self.indent += 1
        node.expr.accept(self)
        self.indent -= 1

    def visit_write_node(self, node):
        self.print_indent("Write:")
        self.indent += 1
        self.print_indent("x_expr:")
        self.indent += 1
        node.x_expr.accept(self)
        self.indent -= 1
        self.print_indent("y_expr:")
        self.indent += 1
        node.y_expr.accept(self)
        self.indent -= 1
        self.print_indent("color_expr:")
        self.indent += 1
        node.color_expr.accept(self)
        self.indent -= 1
        self.indent -= 1

    def visit_write_box_node(self, node):
        self.print_indent("WriteBox:")
        self.indent += 1
        self.print_indent("x_expr:")
        self.indent += 1
        node.x_expr.accept(self)
        self.indent -= 1
        self.print_indent("y_expr:")
        self.indent += 1
        node.y_expr.accept(self)
        self.indent -= 1
        self.print_indent("w_expr:")
        self.indent += 1
        node.w_expr.accept(self)
        self.indent -= 1
        self.print_indent("h_expr:")
        self.indent += 1
        node.h_expr.accept(self)
        self.indent -= 1
        self.print_indent("color_expr:")
        self.indent += 1
        node.color_expr.accept(self)
        self.indent -= 1
        self.indent -= 1

    def visit_clear_node(self, node):
        self.print_indent("Clear:")
        self.indent += 1
        node.color_expr.accept(self)
        self.indent -= 1

    def visit_array_access_node(self, node):
        self.print_indent(f"ArrayAccess: {node.array_name}")
        self.indent += 1
        node.index_expr.accept(self)
        self.indent -= 1

    def visit_array_assignment_node(self, node):
        self.print_indent(f"ArrayAssign: {node.array_name}")
        self.indent += 1
        self.print_indent("Index:")
        self.indent += 1
        node.index_expr.accept(self)
        self.indent -= 1
        self.print_indent("Value:")
        self.indent += 1
        node.value_expr.accept(self)
        self.indent -= 2
        
    def visit_array_declaration_node(self, node):
        self.print_indent(f"ArrayDecl: {node.element_type} {node.name}")
        if node.size_expr:
            self.print_indent("Size:")
            self.indent += 1
            node.size_expr.accept(self)
            self.indent -= 1
        self.print_indent("Initial Values:")
        self.indent += 1
        for val in node.values:
            val.accept(self)
        self.indent -= 1




