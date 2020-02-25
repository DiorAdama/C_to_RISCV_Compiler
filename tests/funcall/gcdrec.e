int gcd(int a,int b){
 if(b == 0){
   return a;
 } else {
   return gcd(b, a % b);
 }
}
int main(int argc,char* argv[]){
  int a = atoi(argv[1]);
  int b = atoi(argv[2]);
  return gcd(a,b);
}
