struct mastruct {
  int x;
  int y;
};

int main(int argc,char* argv[]){
  struct mastruct S;
  S.x = 12;
  S.y = 3;
  return (S.x + S.y);
}
