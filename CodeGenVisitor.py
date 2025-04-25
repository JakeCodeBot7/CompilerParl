from AST import *

class CodeGenVisitor(ASTVisitor):
    def __init__(self):
        self.instructions = []
        self.label_counter = 0
        self.scope_level = 0
        self.frame_index = 0
        self.symbols = [{}]
        self.in_function = False

    def emit(self, line):
        self.instructions.append(line)

    def push_scope(self, size=0):
        # Allocate stack frame with `size` slots
        self.emit(f"push {size}")
        self.emit("oframe")
        self.symbols.append({})
        self.frame_index = 0
        self.scope_level += 1

    def pop_scope(self):
        # Remove current frame
        if self.scope_level > 0:
            self.emit("cframe")
            self.symbols.pop()
            self.scope_level -= 1

    def declare_variable(self, name):
        # Track variable location in current frame
        self.symbols[-1][name] = (self.frame_index, self.scope_level)
        self.frame_index += 1

    def lookup_variable(self, name):
        # Look up variable from innermost to outer scope
        for level in reversed(range(len(self.symbols))):
            if name in self.symbols[level]:
                index, scope_level = self.symbols[level][name]
                return index, scope_level
        raise Exception(f"Undeclared variable '{name}'")

    def new_label(self, base):
        label = f"{base}_{self.label_counter}"
        self.label_counter += 1
        return label

    def visit_program_node(self, node):
        # Reset code buffers
        self.instructions = []
        self.main_instructions = []
        self.function_instructions = []

        self.emit = self.main_instructions.append
        self.emit(".main")

        # Start global scope (frame level -1 will become 0 after push_scope)
        self.symbols = [{}]
        self.frame_index = 0
        self.scope_level = -1

        # Count slots needed for global variables
        global_var_count = 0
        for stmt in node.statements:
            if isinstance(stmt, ASTArrayDeclarationNode):
                size = len(stmt.values) if stmt.size_expr is None else int(stmt.size_expr.value)
                global_var_count += size
            elif isinstance(stmt, ASTVariableDeclarationNode):
                global_var_count += 1

        self.push_scope(size=global_var_count)

        for stmt in node.statements:
            stmt.accept(self)

        self.pop_scope()
        self.emit("halt")

        # Combine all emitted instructions
        self.instructions = self.main_instructions + self.function_instructions
        self.emit = self.instructions.append

    def visit_function_node(self, node):
        old_emit = self.emit
        self.emit = self.function_instructions.append

        self.emit(f".{node.name}")

        # Push function-local scope (for parameters & local vars)
        self.symbols.append({})
        self.scope_level += 1
        self.frame_index = 0

        # Bind parameters into frame
        for i, (_, pname) in enumerate(node.params):
            idx = self.frame_index
            lvl = self.scope_level
            self.symbols[-1][pname] = (idx, lvl)
            self.frame_index += 1

            self.emit(f"push [{i}:{lvl}]")     
            self.emit(f"push {idx}")
            self.emit(f"push {lvl}")
            self.emit("st")

        node.body.accept(self)

        self.symbols.pop()
        self.scope_level -= 1

        self.emit = old_emit

    def visit_block_node(self, node):
        for stmt in node.statements:
            stmt.accept(self)

    def visit_variable_declaration_node(self, node):
        index = self.frame_index
        level = self.scope_level
        self.declare_variable(node.name)

        if node.expr:
            node.expr.accept(self)
        else:
            self.emit("push 0")

        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("st")

    def visit_assignment_node(self, node):
        node.expr.accept(self)
        index, level = self.lookup_variable(node.name)
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("st")

    def visit_literal_node(self, node):
        self.emit(f"push {node.value}")

    def visit_identifier_node(self, node):
        index, level = self.lookup_variable(node.name)
        self.emit(f"push [{index}:{level}]")

    def visit_expression_statement(self, node):
        node.expr.accept(self)

    def visit_binary_op_node(self, node):
        node.left.accept(self)
        node.right.accept(self)

        op_map = {
            "+": "add", "-": "sub", "*": "mul", "/": "div",
            "<": "lt", "<=": "le", ">": "gt", ">=": "ge",
            "==": "eq", "!=": "ne", "and": "and", "or": "or"
        }

        instr = op_map.get(node.operator)
        if not instr:
            raise Exception(f"Unsupported binary operator: {node.operator}")
        self.emit(instr)

    def visit_unary_op_node(self, node):
        node.operand.accept(self)
        if node.operator == "-":
            self.emit("push 0")
            self.emit("sub")
        elif node.operator == "not":
            self.emit("not")
        else:
            raise Exception(f"Unsupported unary operator: {node.operator}")

    def visit_cast_node(self, node):
        node.expression.accept(self)

    def visit_function_call_node(self, node):
        for arg in reversed(node.arguments):
            arg.accept(self)
        self.emit(f"push .{node.name}")
        self.emit("call")

    def visit_if_node(self, node):
        else_label = self.new_label("else")
        end_label = self.new_label("endif")

        node.condition.accept(self)
        self.emit(f"push #{else_label}")
        self.emit("cjmp")

        node.then_block.accept(self)
        self.emit(f"push #{end_label}")
        self.emit("jmp")

        self.emit(f"{else_label}:")
        if node.else_block:
            node.else_block.accept(self)
        self.emit(f"{end_label}:")

    def visit_while_node(self, node):
        start_label = self.new_label("while_start")
        end_label = self.new_label("while_end")

        self.emit(f"{start_label}:")
        node.condition.accept(self)
        self.emit(f"push #{end_label}")
        self.emit("cjmp")

        node.body.accept(self)
        self.emit(f"push #{start_label}")
        self.emit("jmp")
        self.emit(f"{end_label}:")

    def visit_for_node(self, node):
        self.emit("push 1")
        self.emit("oframe")
        if node.init_stmt:
            node.init_stmt.accept(self)

        start_label = self.new_label("for_cond")
        body_label = self.new_label("for_body")
        end_label = self.new_label("for_end")

        self.emit(f"{start_label}:")
        if node.condition_expr:
            node.condition_expr.accept(self)
            self.emit(f"push #{body_label}")
            self.emit("cjmp")
            self.emit(f"push #{end_label}")
            self.emit("jmp")
        else:
            self.emit(f"push #{body_label}")
            self.emit("jmp")

        self.emit(f"{body_label}:")
        self.emit("push 0")
        self.emit("oframe")
        node.body.accept(self)
        self.emit("cframe")

        if node.increment_stmt:
            node.increment_stmt.accept(self)

        self.emit(f"push #{start_label}")
        self.emit("jmp")
        self.emit(f"{end_label}:")
        self.emit("cframe")

    def visit_return_node(self, node):
        node.expr.accept(self)
        self.emit("ret")

    def visit_print_node(self, node):
        node.expr.accept(self)
        self.emit("print")

    def visit_delay_node(self, node):
        node.expr.accept(self)
        self.emit("delay")

    def visit_write_node(self, node):
        node.color_expr.accept(self)
        node.y_expr.accept(self)
        node.x_expr.accept(self)
        self.emit("write")

    def visit_write_box_node(self, node):
        node.color_expr.accept(self)
        node.h_expr.accept(self)
        node.w_expr.accept(self)
        node.y_expr.accept(self)
        node.x_expr.accept(self)
        self.emit("writebox")

    def visit_clear_node(self, node):
        node.color_expr.accept(self)
        self.emit("clear")

    def visit_array_declaration_node(self, node):
        size = len(node.values) if node.size_expr is None else int(node.size_expr.value)

        self.declare_variable(node.name)
        index, level = self.lookup_variable(node.name)

        for val_node in reversed(node.values):
            val_node.accept(self)

        self.emit(f"push {size}")
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("sta")

    def visit_array_access_node(self, node):
        node.index_expr.accept(self)
        index, level = self.lookup_variable(node.array_name)
        self.emit(f"push +[{index}:{level}]")

    def visit_array_assignment_node(self, node):
        node.value_expr.accept(self)
        node.index_expr.accept(self)
        index, level = self.lookup_variable(node.array_name)
        self.emit(f"push {index}")
        self.emit(f"push {level}")
        self.emit("astore")
