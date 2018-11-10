#define GLFW_INCLUDE_GLCOREARB
#define GLFW_INCLUDE_GLU

#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <math.h>

float gravity = 0.00074f;

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
  struct point *subrockets[10];
};

struct point {
  float x;
  float y;
  float angle;
  float speed;
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
  int i = 0;
  float r = 35.0f;
  raket = malloc(sizeof *raket);
  raket->x = 0.0f;
  raket->y = -0.75f;
  raket->angle = to_radian(float_rand(90.0f - r, 90.0f + r));
  raket->r = float_rand(0.0f, 1.0f);
  raket->g = float_rand(0.0f, 1.0f);
  raket->b = float_rand(0.0f, 1.0f);
  (*raket).lifetime = 0; // en alternativ syntax
  for(i = 0; i < 10; i ++) {
    struct point *point;
    raket->subrockets[i] = malloc(sizeof *point);
    raket->subrockets[i]->x = 0.0f;
    raket->subrockets[i]->y = 0.0f;
    raket->subrockets[i]->speed = (1+i)/(float)100000;
    raket->subrockets[i]->angle = to_radian(36 * i);
  }
  return raket;
}

void delete_rocket(struct rocket *raket) {
  int i = 0;
  for(i = 0; i < 10; i ++) {
    free(raket->subrockets[i]);
  }
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
void draw_spot(struct rocket *rock, float extra_x, float extra_y) {
  float rad = 0.008f;
  float x = rock->x + extra_x;
  float y = rock->y + extra_y;
  glBegin(GL_TRIANGLES);
  glColor3f(rock->r, rock->g, rock->b);
  glVertex3f(x, y-rad, 1);
  glVertex3f(x + rad/1.5, y, 1);
  glVertex3f(x, y + rad, 1);
  glVertex3f(x, y+rad, 1);
  glVertex3f(x-rad/1.5, y, 1);
  glVertex3f(x, y-rad, 1);
  glEnd();
}

static void key_callback(GLFWwindow *window,
                         int key,
                         int scancode,
                         int action,
                         int mods) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    add_rocket(theholder);
  }
}

int main(int argc, char** argv) {

  GLFWwindow* window;
  int xpos = -1, ypos;
  int i = 0;
  int matare = 0;

  struct holder *it = NULL;
  struct rocket *therocket;

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

  /* glfwEnable(GL_BLEND); */
  /* glfwBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA); */

  while (!glfwWindowShouldClose(window)) {
    /* Render here */
    int width, height;
    matare = 0;
    glfwGetFramebufferSize(window, &width, &height);
    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);
    it = theholder;
    while(it != NULL) {
      therocket = it->raket;
      if(therocket != NULL) {
        if(therocket->lifetime >= 20000) {
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
        } else if(therocket->lifetime >= 1200) {
          for(i = 0; i < 10; i ++) {
            int time_since_explo = therocket->lifetime - 2100;
            int life = therocket->lifetime;
            float old_x = (therocket->subrockets[i])->x;
            float old_y = (therocket->subrockets[i])->y;
            float angle = (therocket->subrockets[i])->angle;
            float new_x = old_x + (float) cos(angle) * 0.0002;
            float new_y = old_y + (float) sin(angle) * 0.00020;
            new_y -= gravity * time_since_explo * 0.00009;
            (therocket->subrockets[i])->x = new_x;
            (therocket->subrockets[i])->y = new_y;
            draw_spot(therocket, new_x, new_y);
            therocket->lifetime ++;
          }
        } else {
          draw_spot(therocket, 0, 0);
          float old_x = therocket->x;
          float old_y = therocket->y;
          float angle = therocket->angle;
          float sideways_mod = 0.0003 * therocket->lifetime/1000;
          float new_x = old_x + (float) cos(angle) * sideways_mod;
          float vertical_mod = (float) (2600-therocket->lifetime*2.4)/1000000;
          float new_y = old_y + (float) sin(angle) * vertical_mod;
          therocket->x = new_x;
          therocket->y = new_y;
          therocket->lifetime ++;
        }
      }
      it = it->next;
      matare++;
    }
    printf("langd: %d\n", matare);

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
