; ModuleID = 'StockX'

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@0 = private unnamed_addr constant [13 x i8] c"hello world!\00"
@1 = private unnamed_addr constant [13 x i8] c"hello world!\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  ret i32 0
}

define i32 @myfunc() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @1, i32 0, i32 0))
  ret i32 0
}
