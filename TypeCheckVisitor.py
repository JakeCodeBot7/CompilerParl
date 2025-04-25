from AST import *
from SymbolTable import SymbolTable

class TypeCheckerVisitor(ASTVisitor):
    def __init__(self):
        self.symbols = SymbolTable()
        self.current_function_return_type = None

        # Declare built-in functions and their types
        self.symbols.declare("__random_int", ("int", ["int"]))
        self.symbols.declare("__read", ("int", ["int", "int"]))
        self.symbols.declare("__width", ("int", []))
        self.symbols.declare("__height", ("int", []))

    def visit_program_node(self, node):
        for stmt in node.statements:
            stmt.accept(self)

    def visit_function_node(self, node):
        # Gather parameter types
        param_types = []
        for (ptype, pname) in node.params:
            if "[" in ptype:
                element_type = ptype.split("[")[0]
                size = int(ptype.split("[")[1].rstrip("]"))
                param_type = {
                    "type": "array",
                    "element_type": element_type,
                    "size": size
                }
            else:
                param_type = ptype
            param_types.append(param_type)

        # Declare the function itself
        self.symbols.declare(node.name, (node.return_type, param_types))

        self.symbols.push_scope()
        self.current_function_return_type = node.return_type
        self.return_found = False

        # Declare parameters inside function scope
        for (ptype, pname) in node.params:
            if "[" in ptype:
                element_type = ptype.split("[")[0]
                size = int(ptype.split("[")[1].rstrip("]"))
                self.symbols.declare(pname, {
                    "type": "array",
                    "element_type": element_type,
                    "size": size
                })
            else:
                self.symbols.declare(pname, ptype)

        node.body.accept(self)

        # Check if non-void functions have a return
        if self.current_function_return_type != "void" and not self.return_found:
            raise TypeError(f"Missing return statement in function '{node.name}'")

        self.symbols.pop_scope()

    def visit_variable_declaration_node(self, node):
        if node.expr:
            expr_type = node.expr.accept(self)
            if expr_type != node.var_type:
                raise TypeError(f"Type mismatch: Cannot assign {expr_type} to {node.var_type}")
        self.symbols.declare(node.name, node.var_type)

    def visit_identifier_node(self, node):
        return self.symbols.lookup(node.name)

    def visit_literal_node(self, node):
        return node.literal_type

    def visit_assignment_node(self, node):
        declared_type = self.symbols.lookup(node.name)
        if declared_type is None:
            raise TypeError(f"Undeclared variable '{node.name}'")
        
        expr_type = node.expr.accept(self)
        if declared_type != expr_type:
            raise TypeError(f"Type mismatch in assignment to '{node.name}': expected {declared_type}, got {expr_type}")

    def visit_binary_op_node(self, node):
        left_type = node.left.accept(self)
        right_type = node.right.accept(self)

        if left_type != right_type:
            raise TypeError(f"Type mismatch in binary operation: {left_type} vs {right_type}")

        if node.operator in {"<", ">", "<=", ">=", "==", "!="}:
            return "bool"
        else:
            return left_type

    def visit_unary_op_node(self, node):
        operand_type = node.operand.accept(self)
        if node.operator == "not" and operand_type != "bool":
            raise TypeError(f"'not' operator expects bool, got {operand_type}")
        if node.operator == "-" and operand_type not in {"int", "float"}:
            raise TypeError(f"Unary '-' operator expects int/float, got {operand_type}")
        return operand_type

    def visit_cast_node(self, node):
        node.expression.accept(self)  # Assume cast is always allowed
        return node.target_type

    def visit_if_node(self, node):
        cond_type = node.condition.accept(self)
        if cond_type != "bool":
            raise TypeError("Condition in 'if' must be boolean")
        node.then_block.accept(self)
        if node.else_block:
            node.else_block.accept(self)

    def visit_for_node(self, node):
        if node.init_stmt:
            node.init_stmt.accept(self)
        if node.condition_expr:
            cond_type = node.condition_expr.accept(self)
            if cond_type != "bool":
                raise TypeError("For loop condition must be bool")
        if node.increment_stmt:
            node.increment_stmt.accept(self)
        node.body.accept(self)

    def visit_while_node(self, node):
        cond_type = node.condition.accept(self)
        if cond_type != "bool":
            raise TypeError("Condition in 'while' must be boolean")
        node.body.accept(self)

    def visit_return_node(self, node):
        ret_type = node.expr.accept(self)
        if ret_type != self.current_function_return_type:
            raise TypeError(f"Return type mismatch: Expected {self.current_function_return_type}, got {ret_type}")
        self.return_found = True  # Track that a return was found

    def visit_block_node(self, node):
        self.symbols.push_scope()
        for stmt in node.statements:
            stmt.accept(self)
        self.symbols.pop_scope()

    def visit_function_call_node(self, node):
        func_data = self.symbols.lookup(node.name)
        if func_data is None:
            raise TypeError(f"Undeclared function '{node.name}'")

        return_type, param_types = func_data

        if len(node.arguments) != len(param_types):
            raise TypeError(
                f"Function '{node.name}' expects {len(param_types)} arguments, "
                f"but got {len(node.arguments)}"
            )

        # Check each argument type
        for i, arg in enumerate(node.arguments):
            arg_type = arg.accept(self)
            expected_type = param_types[i]

            if isinstance(expected_type, dict) and expected_type.get("type") == "array":
                if not (
                    isinstance(arg_type, dict) and
                    arg_type.get("type") == "array" and
                    arg_type.get("element_type") == expected_type.get("element_type") and
                    arg_type.get("size") == expected_type.get("size")
                ):
                    raise TypeError(
                        f"Argument {i+1} of function '{node.name}' expected array of type {expected_type}, but got {arg_type}"
                    )
            else:
                if arg_type != expected_type:
                    raise TypeError(
                        f"Argument {i+1} of function '{node.name}' expected {expected_type}, but got {arg_type}"
                    )

        return return_type

    # ----------------------------------
    # Built-in command nodes
    # ----------------------------------

    def visit_print_node(self, node):
        node.expr.accept(self)
        return None

    def visit_delay_node(self, node):
        node.expr.accept(self)
        return None

    def visit_write_node(self, node):
        node.x_expr.accept(self)
        node.y_expr.accept(self)
        node.color_expr.accept(self)
        return None

    def visit_write_box_node(self, node):
        node.x_expr.accept(self)
        node.y_expr.accept(self)
        node.w_expr.accept(self)
        node.h_expr.accept(self)
        node.color_expr.accept(self)
        return None

    def visit_clear_node(self, node):
        node.color_expr.accept(self)
        return None

    def visit_expression_statement(self, node):
        node.expr.accept(self)
        return None

    # ----------------------------------
    # Arrays
    # ----------------------------------

    def visit_array_declaration_node(self, node):
        array_size = len(node.values)

        if node.size_expr:
            size_type = node.size_expr.accept(self)
            if size_type != "int":
                raise TypeError("Array size must be an integer")

            declared_size = int(node.size_expr.value)
            if declared_size != array_size:
                raise TypeError(f"Array initializer has {array_size} values but declared size is {declared_size}")
        else:
            declared_size = array_size

        for value_node in node.values:
            val_type = value_node.accept(self)
            if val_type != node.element_type:
                raise TypeError(f"Expected {node.element_type} in array, got {val_type}")

        self.symbols.declare(node.name, {
            "type": "array",
            "element_type": node.element_type,
            "size": declared_size
        })

    def visit_array_access_node(self, node):
        symbol = self.symbols.lookup(node.array_name)
        if not isinstance(symbol, dict) or symbol.get("type") != "array":
            raise TypeError(f"{node.array_name} is not an array")

        index_type = node.index_expr.accept(self)
        if index_type != "int":
            raise TypeError("Array index must be an integer")

        return symbol["element_type"]

    def visit_array_assignment_node(self, node):
        symbol = self.symbols.lookup(node.array_name)
        if symbol["type"] != "array":
            raise TypeError(f"{node.array_name} is not an array")

        index_type = node.index_expr.accept(self)
        if index_type != "int":
            raise TypeError("Array index must be an integer")

        value_type = node.value_expr.accept(self)
        if value_type != symbol["element_type"]:
            raise TypeError(f"Expected {symbol['element_type']} but got {value_type}")
