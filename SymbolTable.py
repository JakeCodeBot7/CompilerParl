
class SymbolTable:
    def __init__(self):
        self.scopes = [{}]  # stack of dictionaries

    def push_scope(self):
        self.scopes.append({})

    def pop_scope(self):
        if len(self.scopes) > 1:
            self.scopes.pop()
        else:
            raise Exception("Cannot pop global scope")

    def declare(self, name, var_type):
        if name in self.scopes[-1]:
             raise TypeError(f"Variable '{name}' already declared in this scope")
        self.scopes[-1][name] = var_type


    def lookup(self, name):
        for scope in reversed(self.scopes):
            if name in scope:
                return scope[name]
        raise Exception(f"Undeclared identifier: '{name}'")

    def __str__(self):
        return str(self.scopes)
