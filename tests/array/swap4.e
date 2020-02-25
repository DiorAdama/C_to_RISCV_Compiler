int swap(int* t,int i,int j){
  int tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
  return 0;
}
int main(int argc,char* argv[]){
  int t[10];
  t[0] = 12;
  t[1] = 5;
  swap(t, 0,1);
  int j = 0;
    if(t[0] <= t[1]){
      swap(t, 0, 0);
      j = j + 1;
    } else {
    }
  swap(t, 1, 1);
  print (j);
  print (t[0]);
  print (t[1]);
  return 0;
}

