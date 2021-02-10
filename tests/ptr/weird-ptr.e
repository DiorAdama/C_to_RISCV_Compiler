int ignore(int * x){
  return 0;
}
int main(){
  int x = 0;
  int y = 0;
  ignore(&x); // force x on the stack
  int* p = &y - 1;
  *p = 3;
  return x;
}
