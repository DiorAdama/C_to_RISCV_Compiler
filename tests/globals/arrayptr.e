struct S {
  int x;
  int y;
  int z;
};

struct S *p;

struct S t[3];

int main(){
  (t[0]).x = 1;
  (t[0]).y = 2;
  (t[0]).z = 3;
  (t[1]).x = 4;
  (t[1]).y = 5;
  (t[1]).z = 6;
  (t[2]).x = 7;
  (t[2]).y = 8;
  (t[2]).z = 9;
  p = &(t[2]);

  return (*p).y;
}
