int IMG[64];
int NBCOL = 8;
int NBROW = 8;

void draw(int color, int x, int y){
  int pos = (y * NBCOL + x);
  if (pos < NBCOL * NBROW){
	  *(IMG + pos) = color;
  }
}

int get(int x, int y){
  int pos = (y * NBCOL + x);
  if (pos < NBCOL * NBROW){
    return *(IMG + pos);
  }
  return -1;
}

void draw_world(){
  int i = 0;
  int j = 0;
  while (j < NBROW) {
    i = 0;
    while (i < NBCOL) {
      if(get(i, j) != 0){
        print_char('#');
      } else {
        print_char(' ');
      }
      i = i + 1;
    }
    print_char('\n');
    j = j + 1;
  }
}

int main(){
  draw(0xff0000, 1, 1);
  print(get(1,1));
  draw_world();
  return 0;
}
