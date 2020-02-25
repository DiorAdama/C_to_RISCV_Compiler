int main(int argc,char* argv[]){
  int x = atoi(argv[1]);
  int y = atoi(argv[2]);
  return f(x,x,y,y,x*2);
}
int f(int x,int y,int z,int t,int u){
  return x + y + z + t + u;
}
