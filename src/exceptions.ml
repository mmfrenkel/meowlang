(************ Exceptions ************)

(* Expression/Statement  Exceptions *)
exception DuplicateIdentifier of string
exception IllegalAssignment
exception VariableNotFound of string
exception IllegalBinaryOp of string
exception IllegalUnaryOp of string
exception VariableAssignmentError of string
exception ControlFlowIllegalArgument of string

(* Function Exceptions *)
exception MissingMainFunction of string
exception FunctionNotFound of string
exception FunctionArgumentLengthMismatch of string
exception ArgumentTypeMismatch of string
exception ReturnFromVoidFunction of string
exception ReturnTypeInvalid of string

(* Object Exceptions *)
exception ClassNotFound of string
exception InstanceVariableNotFound of string
exception InvalidDealloc of string
exception InvalidClassMemberAssignment of string
exception ClassMethodNotFound of string
exception MethodArgumentLengthMismatch of string
exception InvalidMethodCall of string
exception ObjectCreationInvalid of string
exception ObjectConstructorInvalid of string
exception ObjectInstanceVariableInvalid of string
exception InstanceVariableAccessInvalid of string
exception UnknownStruct of string

(* Array Exceptions *)
exception InvalidArraySizeSpecified of string
exception InvalidArrayItem of string
exception InvalidArrayAccess of string
exception ExcessArrayInput of string
exception InvalidArrayAssignment of string

(* Other *)
exception NotYetSupported of string
exception InternalError of string


(* Message Templates for Exceptions *)
let dup_func_msg                = "duplicate function name, or conflict with built-in:"
let dup_class_msg               = "duplicate class name:"
let dup_method_msg              = "duplicate class method name:"
let dup_instance_var_msg        = "duplicate instance variable name identified in class declaration:"
let dup_formal_msg              = "duplicate formal name :"
let dup_local_var_msg           = "duplicate local variable name :"
let dup_form_local_msg          = "duplicate identifers (formal and local) name :"
let dup_form_instance_msg       = "duplicate identifier (formal and local conflict with instance vars)"
let undeclared_msg              = "undeclared identifier: "
let assignment_typ_mismatch     = "variables can only be assigned to items of the expected type"
let missing_main_func_msg       = "all programs must have a 'Main' function"
let func_arg_num_mismatch       =  "expected different number of arguments for function: "
let meth_arg_num_mismatch       =  "expected different number of arguments for method: "
let expr_type_mismatch          = "expression type mismatch: " (* expected _ but got _ in expression _ *)
let op_type_mismatch_inc_dec    = "operation type mismatch: expected Increment or Decrement but got type"
let op_type_mismatch_boolean    = "operation type mismatch: expected Boolean but got type"
let op_type_mismatch_int        = "operation type mismatch: expected Integer but got type"
let op_type_mismatch_loop_term  = "operation type mismatch: expected <, >, =, != as loop termination condition:"
let class_method_unknown        = "method does not exist for this class:"
let invalid_method_call         = "methods can only be called on objects:"
let invalid_array_size_msg      = "arrays sizes must be integer literals or variables only:"
let invalid_object_creation     = "you can only create objects from classes"
let invalid_instance_var_access = "instance variables only exist in classes"
let invalid_array_item_msg      = "array elements must be of specified type for the array"
let excess_array_item           = "array contents exceeded specified array size"
let invalid_deallocation_msg    = "BLEEP (free) can only be called on variables of object or array type that have been allocated:"
let invalid_cls_member_assign   = "you must assign valid instance variables within a class with items of valid type: tried to assign"
let member_assign_cls_only      = "you can only assign instance variables for class objects:"
let array_access_array_only     = "array indexing is only available for array types"
let array_access_integer        = "arrays can only be indexed with integer types (>= 0):"
let array_access_out_of_bounds  = "index exceeds array size"
let object_constructor_error    = "to assign instance variables on object creation, you must use assignment expressions"
let object_constructor_types    = "you may only assign instance variables that are defined within the class and that are of the correct type"
let use_of_this_outside_class   = "use of HERE ('this') keyword can only be used inside of a class to refer to its own methods/variables"
let class_access_msg            = "class access is performed on an object identified by a variable"
let method_call_expr            = "attempting to call method on invalid expression; must be variable representing an object type or an indexed array of objects"
let return_from_void_func       = "attempting to return from a void function"
let return_type_invalid         = "attempting to return value that doesn't match function return type"
let scan_error                  = "built in scan function takes only the id of a string as an argument"