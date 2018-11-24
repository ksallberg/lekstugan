#define GLFW_INCLUDE_GLCOREARB
#define GLFW_INCLUDE_GLU
#define TRAIL 15

#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>

float gravity = 0.00074f;
int explode_time = 320;
int remove_time = 2000;
int run = 1;

struct holder {
  struct rocket *raket;
  struct holder *next;
};

struct rocket {
  float x;
  float y;
  float angle;
  int lifetime;
  float r;
  float g;
  float b;
  float spread;
  struct point *myself;
  struct point *subrockets[10];
};

struct point {
  float x;
  float y;
  float angle;
  float speed;
  float prevx[TRAIL];
  float prevy[TRAIL];
};

// https://stackoverflow.com/questions/13408990/ \
//    how-to-generate-random-float-number-in-c
float float_rand(float min, float max) {
  float scale = rand() / (float) RAND_MAX;
  return min + scale * (max - min);
}

float to_radian(float degree) {
  return degree * M_PI / 180.0;
}

struct rocket *new_rocket() {
  struct rocket *raket;
  int i, j = 0;
  int normal = 1;
  float r = 35.0f;
  struct point *point;
  raket = malloc(sizeof *raket);
  raket->x = 0.0f;
  raket->y = -0.75f;
  raket->angle = to_radian(float_rand(90.0f - r, 90.0f + r));
  raket->r = float_rand(0.0f, 1.0f);
  raket->g = float_rand(0.0f, 1.0f);
  raket->b = float_rand(0.0f, 1.0f);
  raket->myself = malloc(sizeof *point);
  raket->spread = float_rand(0.0003f, 0.0035f);
  for(j = 0; j < TRAIL; j ++) {
    raket->myself->prevx[j] = -100.0f;
    raket->myself->prevy[j] = -100.0f;
  }
  (*raket).lifetime = 0; // en alternativ syntax

  if(rand() % 7 == 0) {
    normal = 0;
  }

  for(i = 0; i < 10; i ++) {
    raket->subrockets[i] = malloc(sizeof *point);
    raket->subrockets[i]->x = 0.0f;
    raket->subrockets[i]->y = 0.0f;
    /* raket->subrockets[i]->speed = (1+i)/(float)100000; */

    if(!normal) {
      raket->subrockets[i]->angle = to_radian(36 * i + rand());
    } else {
      raket->subrockets[i]->angle = to_radian(36 * i);
    }
    for(j = 0; j < TRAIL; j ++) {
      raket->subrockets[i]->prevx[j] = -100.0f;
      raket->subrockets[i]->prevy[j] = -100.0f;
    }
  }
  return raket;
}

void delete_rocket(struct rocket *raket) {
  int i = 0;
  for(i = 0; i < 10; i ++) {
    free(raket->subrockets[i]);
  }
  free(raket->myself);
  free(raket);
}

void delete_holder(struct holder *hold) {
  struct rocket *therocket = hold->raket;
  delete_rocket(therocket);
  free(hold);
}

struct rocket *therocket = NULL;
struct holder *theholder = NULL;

void add_rocket(struct holder *hold) {
  struct rocket *newraket = NULL;
  struct holder *it = hold;
  struct holder *newnext = NULL;
  newraket = new_rocket();
  if(it->raket == NULL) {
    it->raket= newraket;
  } else {
    while(it->next != NULL) {
      it = it->next;
    }
    newnext = malloc(sizeof *newnext);
    newnext->raket = newraket;
    newnext->next = NULL;
    it->next = newnext;
  }
}

/* Draw a little diamond at a certain coordinate */
void draw_spot(struct rocket *rock,
               float extra_x, float extra_y, float alpha) {
  float rad = 0.0055f;
  float x = rock->x + extra_x;
  float y = rock->y + extra_y;
  float alpha2 = alpha - (((float) rock->lifetime - explode_time/40)/ 700);
  glBegin(GL_TRIANGLES);
  glColor4f(rock->r, rock->g, rock->b, alpha2);
  glVertex3f(x, y-rad, 1);
  glVertex3f(x + rad/1.5, y, 1);
  glVertex3f(x, y + rad, 1);
  glVertex3f(x, y+rad, 1);
  glVertex3f(x-rad/1.5, y, 1);
  glVertex3f(x, y-rad, 1);
  glEnd();
}

/* Draw a little diamond at a certain coordinate */
void draw_pt(struct point *pt, float alpha) {
  float rad = 0.0055f;
  float x = pt->x;
  float y = pt->y;
  glBegin(GL_TRIANGLES);
  glColor4f(1.0f, 0.5f, 0.5f, alpha);
  glVertex3f(x, y-rad, 1);
  glVertex3f(x + rad/1.5, y, 1);
  glVertex3f(x-rad/1.5, y + rad, 1);
  glEnd();
}

static void key_callback(GLFWwindow *window,
                         int key,
                         int scancode,
                         int action,
                         int mods) {
  if (key == GLFW_KEY_SPACE) {
    add_rocket(theholder);
  } else if (key == GLFW_KEY_ESCAPE) {
    run = 0;
  }
}

int main(int argc, char** argv) {

  GLFWwindow* window;
  int xpos = -1, ypos;
  int i = 0;

  struct holder *it = NULL;
  struct rocket *therocket;

  double previousTime = glfwGetTime();
  int frameCount = 0;

  /* Initialize the library */
  if ( !glfwInit() ) {
     return -1;
  }

  theholder = malloc(sizeof *theholder);
  theholder->raket = NULL;
  theholder->next = NULL;

  /* Create a windowed mode window and its OpenGL context */
  window = glfwCreateWindow(1280, 720, "Hello World", NULL, NULL);

  if (!window) {
     glfwTerminate();
     return -1;
  }

  /* Make the window's context current */
  glfwMakeContextCurrent(window);
  glfwSwapInterval(1);
  glfwSetKeyCallback(window, key_callback);
  /* Loop until the user closes the window */

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  while (!glfwWindowShouldClose(window) && run == 1) {
    /* Render here */
    int width, height;


    // Measure speed
    double currentTime = glfwGetTime();
    frameCount++;
    // If a second has passed.
    if ( currentTime - previousTime >= 1.0 )
    {
        // Display the frame count here any way you want.
        printf("FPS: %d\n", frameCount);

        frameCount = 0;
        previousTime = currentTime;
    }

    #ifdef __APPLE__
      usleep(14000);
    #endif

    glfwGetFramebufferSize(window, &width, &height);
    /* glViewport(0, 0, width, height); */
    glClear(GL_COLOR_BUFFER_BIT);
    it = theholder;
    while(it != NULL) {
      therocket = it->raket;
      if(therocket != NULL) {
        if(therocket->lifetime >= remove_time) {
          struct holder *temp = theholder->next;
          /* specialfall, om det bara finns en holder,
             ska vi inte ta bort den for det sabbar
             antagandet att det alltid finns en
             -> ta istallet bort raketen inuti

             (man hade istallet kunnat allokera en
              ny holder sedan nar man lagger till
              en ny raket nasta gang)
          */
          if(theholder->next == NULL) {
            delete_rocket(therocket);
            theholder->raket=NULL;
          } else {
            delete_holder(theholder);
            theholder = temp;
          }
          /* theholder = temp; */
          /* it = temp; */
        } else if(therocket->lifetime >= explode_time) {
          for(i = 0; i < 10; i ++) {
            int time_since_explo = therocket->lifetime - explode_time;
            int life = therocket->lifetime;
            float old_x = (therocket->subrockets[i])->x;
            float old_y = (therocket->subrockets[i])->y;
            float angle = (therocket->subrockets[i])->angle;
            float new_x = old_x + (float) cos(angle) * therocket->spread;
            float new_y = old_y + (float) sin(angle) * therocket->spread;

            // trail effect
            (therocket->subrockets[i])->prevx[TRAIL-1] = old_x;
            (therocket->subrockets[i])->prevy[TRAIL-1] = old_y;

            if(therocket->lifetime % 4 == 0) {
              for(int j=0; j < TRAIL-1; j++) {
                (therocket->subrockets[i])->prevy[j] =
                  (therocket->subrockets[i])->prevy[j+1];

                (therocket->subrockets[i])->prevx[j] =
                  (therocket->subrockets[i])->prevx[j+1];
              }
            }

            for(int j=0; j < TRAIL; j++) {
              draw_spot(therocket,
                        (therocket->subrockets[i])->prevx[j],
                        (therocket->subrockets[i])->prevy[j],
                        (float) j / 5.0f);
            }
            new_y -= gravity * time_since_explo * 0.0009;
            (therocket->subrockets[i])->x = new_x;
            (therocket->subrockets[i])->y = new_y;
            draw_spot(therocket, new_x, new_y, 1.0f);

            therocket->lifetime ++;
          }
        } else {
          float old_x = therocket->x;
          float old_y = therocket->y;
          float angle = therocket->angle;
          float sideways_mod = 0.003 * therocket->lifetime/300;
          float new_x = old_x + (float) cos(angle) * sideways_mod;
          float vertical_mod =
            (float) (explode_time-therocket->lifetime*1.2)/30000;
          float new_y = old_y + (float) sin(angle) * vertical_mod;
          int j = 0;
          therocket->x = new_x;
          therocket->y = new_y;
          therocket->lifetime ++;

          // trail effect
          (therocket->myself)->prevx[TRAIL-1] = old_x;
          (therocket->myself)->prevy[TRAIL-1] = old_y;

          if(therocket->lifetime % 2 == 0) {
            for(int j=0; j < TRAIL-1; j++) {
              (therocket->myself)->prevy[j] =
                (therocket->myself)->prevy[j+1];

              (therocket->myself)->prevx[j] =
                (therocket->myself)->prevx[j+1];
            }
          }

          struct point pp;
          for(int j=0; j < TRAIL; j++) {
            pp.x = (therocket->myself)->prevx[j];
            pp.y = (therocket->myself)->prevy[j];
            draw_pt(&pp, (float) j / 55.0f);
          }

          draw_spot(therocket, 0, 0, 1.0f);
        }
      }
      it = it->next;
    }

    /* Swap front and back buffers */
    glfwSwapBuffers(window);

    /* Poll for and process events */
    glfwPollEvents();

    if(xpos == -1) {
      glfwGetWindowPos(window, &xpos, &ypos);
      glfwSetWindowPos(window, xpos+1, ypos);
      glfwSetWindowPos(window, xpos, ypos);
    }
  }

  glfwTerminate();
  free(theholder);
  return 0;
}
