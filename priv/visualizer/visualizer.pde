/**
 * Cloud simulation
 */

int sx, sy;
int gridsize_x, gridsize_y;
float mingraph, maxgraph, ellipsecutoff;
Drop[] drops;
float globalmaxdrop;
BufferedReader reader;
float dropscale;
String line;

void setup()
{
  sx = 200;
  sy = 800;
  globalmaxdrop = 0;
  gridsize_x = 50;
  gridsize_y = 200;
  mingraph = 0.01;
  maxgraph = 100;
  ellipsecutoff = 0.05;
  dropscale = 2 * 1/mingraph;
  dropscale = 28;
  frameRate(15);
  reader = createReader("data/run_0.001_5000_2000_1_0.42_50_200");
  //reader = createReader("data/run_0.001_1000_2000_0.1_0.54_50_200");
  //reader = createReader("data/run_0.001_1000_2000_0.5_0.9_50_200");
  //reader = createReader("data/run_0.001_1000_2000_1_1.2_50_200");
  //reader = createReader("data/run_0.001_500_2000_1_1.5_50_200");
  //reader = createReader("data/run_0.001_0_5000_1_2_50_200");

  size(sx, sy, P2D);
  fill(#8874FF);
}

void draw()
{
  //  background(0);
  try {
    line = reader.readLine();
  } 
  catch (IOException e) {
    e.printStackTrace();
    line = null;
  }
  if (line == null) {
    // Stop reading because of an error or file is empty
    noLoop();
  } 
  else {
    String[][] pieces = matchAll(line, "(\\d+) (\\d+) (\\d+\\.\\d+)");
    Drop[] drops = parseLine(pieces);
    background(255);
    float maxdrop = 0;
    float mindrop = 1;
    for (int i=0; i < drops.length; i++) {
      if (drops[i].r > mingraph && drops[i].r < maxgraph) {
        if (drops[i].r < ellipsecutoff) {
          point((sx / gridsize_x) * drops[i].x,
                 sy - (sy / gridsize_y) * drops[i].y);
        } else {
          if (drops[i].r > 2) {
            fill(#FF3131);
          } else if (drops[i].r > 1) {
            fill(#C748FF);
          } else if (drops[i].r > 0.5) {
            fill(#8874FF);
          } else {
            fill(#C1D0F5);
          }
          ellipse((sx / gridsize_x)*drops[i].x, 
             // y = 0 is at the top, we must flip our coordinates
                  sy - (sy / gridsize_y *drops[i].y), 
                  dropscale * drops[i].r, dropscale * drops[i].r
        //        dropscale * 1, dropscale * 1);
          );
        }
      }
      if (drops[i].r > maxdrop) { 
        maxdrop = drops[i].r;
        if (maxdrop > globalmaxdrop) {
          globalmaxdrop = maxdrop;
        }
      }
      if (drops[i].r < mindrop) { 
        mindrop = drops[i].r;
      }
    }
    println(drops.length + "GMax:" + globalmaxdrop+ " Max:" + maxdrop + " Min:" + mindrop);
    //noLoop();
  }
}

Drop[] parseLine(String[][] pieces) {
  Drop[] drops = new Drop[pieces.length];
  for (int i=0; i < pieces.length; i++) {
    drops[i] = new Drop(float(pieces[i][1]), float(pieces[i][2]), float(pieces[i][3]));
  }
  return drops;
}

class Drop {
  float x, y, r;

  // default constructor
  Drop() {
  }

  Drop(float x, float y, float r) {
    this.x = x;
    this.y = y;
    this.r = r;
  }
}

