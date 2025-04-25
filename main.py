from Parser import Parser
from AST import ASTPrinter
from TypeCheckVisitor import TypeCheckerVisitor
from CodeGenVisitor import CodeGenVisitor
#Works
code = '''
    let x:int = 5;
    __print x; 
'''
#Errors (array works)
code2 = '''
fun MaxInArray(x: int[8]) -> int {
    let m: int = 0;
    for (let i: int = 0; i < 8; i = i + 1) {
        if (x[i] > m) {
            m = x[i];
        }
    }
    return m;
}

let list_of_integers: int[] = [23, 54, 3, 65, 99, 120, 34, 21];
let max: int = MaxInArray(list_of_integers);
__print max;



'''
#Works
code3 = '''
fun giga()-> int {
    return 5;
    }
let x:int = giga();
__print x; 
'''
 #Errors
code4 = '''
fun add(a: int, b: int) -> int {
    return a + b;
}

let result: int = add(10, 32);
__print result;
'''

#Works
code5 = ''' 
let nums: int[5] = [10, 20, 30, 40, 50];
let x: int = nums[2];
__print x;

'''
#Works
code6 = '''   
let a: int = 10 + 5 * 2;
let b: int = (10 + 5) * 2;
let c: int = a - b;
let d: int = a / 3 + b * 2 - c;
__print d;
'''

parser = Parser(code5)
print("TOKENS:")
for token in parser.tokens:
    print(token)
ast = parser.parse()

printer = ASTPrinter()
print("AST:")
ast.accept(printer)

type_checker = TypeCheckerVisitor()
ast.accept(type_checker)

codegen = CodeGenVisitor()
ast.accept(codegen)

print("Generated ParIR:")
for instr in codegen.instructions:
    print(instr)