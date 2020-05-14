; ModuleID = 'MicroC'
source_filename = "MicroC"

%List = type { i32, %ListNode*, %ListNode* }
%ListNode = type { i32, %ListNode* }

declare i32 @printf(i8*, ...)

declare i32 @add(%List*)

declare void @list_init(%List*)

declare %List* @list_create()

declare void @list_append(%List*, i32)

define i32 @main() {
entry:
  %list_create = call %List* @list_create()
  %a = load %List, %List* %list_create
  %add = call i32 @add(%List* %a)
  ret i32 0
}
