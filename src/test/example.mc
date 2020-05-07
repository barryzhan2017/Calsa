/* The GCD algorithm in MicroC */
int a = 200 + 400;
int b = 100;


int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int c;
int d;

float gcdf(float a, float b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x = 10+2;
  int y = 5*30;
  print(x);
  print(y);
  print(a);
  print(b);
  print(c);
  a = 10;
  b = 5;
  print(a);
  print(b);
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));
  print(20+100*3);
  return 0;
}
