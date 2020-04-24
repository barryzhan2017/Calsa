/* The GCD algorithm in MicroC */
int a;
int b;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

float gcdf(float a, float b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x;
  int y;
  float c;
  float d;
  a = 18;
  b = 9;
  x = 2;
  y = 14;
  c = 2.0;
  d = 14.0;
  print(gcd(x,y));
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));
  print(1.0);
  print(gcdf(c,d));
  return 0;
}
