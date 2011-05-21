/**
 * Cloud simulation
 */

int sx, sy;
int gridsize_x, gridsize_y;
Drop[] drops;
BufferedReader reader;
String line;

void setup()
{
  size(600, 600, P2D);
  gridsize_x = 200;
  gridsize_y = 200;
  frameRate(8);
  sx = width;
  sy = height;
  reader = createReader("run");
  fill(#8874FF);
}

void draw()
{
//  background(0);
  try {
    line = reader.readLine();
  } catch (IOException e) {
    e.printStackTrace();
    line = null;
  }
  if (line == null) {
    // Stop reading because of an error or file is empty
    noLoop();
  } else {
    String[][] pieces = matchAll(line, "(\\d+) (\\d+) (\\d+\\.\\d+)");
    Drop[] drops = parseLine(pieces);
    background(255);
    for (int i=0; i < drops.length; i++) {
      ellipse((sx / gridsize_x) * drops[i].x, (sy / gridsize_y) * drops[i].y, 10*drops[i].r, 10*drops[i].r);
    }
  }
}

Drop[] parseLine(String[][] pieces) {
  Drop[] drops = new Drop[pieces.length];
  for (int i=0; i < pieces.length; i++) {
      drops[i] = new Drop(float(pieces[i][1]),float(pieces[i][2]), float(pieces[i][3]));
  }
  return drops;
}

class Drop{
  float x, y, r, m;

  // default constructor
  Drop() {
  }

  Drop(float x, float y, float r) {
    // println("New drop: " + x + ", " + y + ", " + r);
    this.x = x;
    this.y = y;
    this.r = r;
    m = r*.1;
  }
}
