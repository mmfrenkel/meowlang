(* Exceptions *)
exception DuplicateIdentifier of string
exception IllegalAssignment
exception MissingMainFunction of string
exception ClassNotFound of string
exception FunctionNotFound of string
exception VariableNotFound of string
exception IllegalBinaryOp of string
exception IllegalUnaryOp of string
exception FunctionArgumentLengthMismatch of string
exception VariableAssignmentError of string
exception FunctionArgumentTypeMismatch of string
exception ControlFlowIllegalArgument of string
exception ClassMethodNotFound of string
exception MethodArgumentLengthMismatch of string
exception InvalidMethodCall of string
exception InvalidArraySizeSpecified of string
exception ObjectCreationInvalid of string
exception InstanceVariableAccessInvalid of string
exception InvalidArrayItem of string
exception ExcessArrayInput of string
exception NotYetSupported of string
exception InternalError of string

(* Message Templates for Exceptions *)
let dup_import_msg = "duplicate import name: "
let dup_func_msg = "duplicate function name, or conflict with built-in: "
let dup_class_msg = "duplicate class name: "
let dup_method_msg = "duplicate class method name: "
let dup_formal_msg = "duplicate formal name :"
let dup_local_var_msg = "duplicate local variable name :"
let dup_form_local_msg = "duplicate identifers (formal and local) name : "
let undeclared_msg = "undeclared identifier: "
let assignment_typ_mismatch = "variables can only be assigned to items of the expected type: "
let missing_main_func_msg = "all programs must have a 'Main' function"
let func_arg_num_mismatch =  "expected different number of arguments for function: "
let meth_arg_num_mismatch =  "expected different number of arguments for method: "

let expr_type_mismatch = "expression type mismatch: " (* expected _ but got _ in expression _ *)

let op_type_mismatch = "operation type mismatch: " (* expected _ but got _ in expression _ *)
let class_method_unknown = "method does not exist for this class: "
let invalid_method_call = "methods can only be called on objects: "
let invalid_array_size_msg = "arrays sizes must be integer literals or variables only: "
let invalid_object_creation = "you can only create objects from classes: "
let invalid_instance_var_access = "instance variables only exist in classes: "
let invalid_array_item_msg = "arrays can only be of the specified type: "
let excess_array_item = "array contents exceeded specified array size: "