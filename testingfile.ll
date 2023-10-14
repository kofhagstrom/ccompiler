define i32 @main() {
  %tmp = add i32 2, 2
  %tmp = add i32 %tmp, 2
  ret i32 %tmp
}