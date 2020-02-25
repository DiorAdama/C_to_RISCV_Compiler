int main(int argc,char* argv[]){
  int ndisque = atoi(argv[1]);
  return hanoi(ndisque);
}
int hanoi(int n)
{
  if(n > 0)
    {
      int x = hanoi(n-1);
      return 1 + 2 * x;
    }
  return 0;
}
