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
  int x = 2;
  int y = 14;
  float c;
  float d;
  a = 18;
  b = 9;
  c = 2.0;
  d = 14.0;
  print(gcd(x,y));
  x = 3;
  y = 15;
  print(gcd(x,y));

  int i = 10;
  int j = 15;
  print(gcd(i, j));
  
  print(gcd(a,b));
  print(1.0);
  print(gcdf(c,d));
  return 0;
}
