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
  int begin = 0;
  int end = 1;
  int pivot = 0;
  swap(t, pivot, end);
  int j = begin;
  int i = begin;
  while(i <= end - 1){
    if(t[i] <= t[end]){
      swap(t, i, j);
      j = j + 1;
    } else {
    }
    i = i + 1;
  }
  swap(t, end, j);
  print (j);
  print (t[0]);
  print (t[1]);
  return 0;
}

