#define GLFW_INCLUDE_GLCOREARB
#define GLFW_INCLUDE_GLU

#include <GLFW/glfw3.h>
#include <stdlib.h>
#include <math.h>

float gravity = 0.00004f;

struct holder {
  struct rocket *raket;
  struct holder *next;
};

struct rocket {
  float x;
  float y;
  float dir;
  int lifetime;
  struct point *subrockets[10];
};

struct point {
  float x;
  float y;
  float angle;
  float speed;
};

float to_radian(float degree) {
  return degree * M_PI / 180.0;
}

struct rocket *new_rocket() {
  struct rocket *raket;
  int i = 0;
  raket = malloc(sizeof *raket);
  raket->x = 0.0f;
  raket->y = -0.75f;
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
void draw_spot(float x, float y) {
  float rad = 0.008f;
  glBegin(GL_TRIANGLES);
  glColor3f(1.f, .5f, .5f);
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
        } else if(therocket->lifetime >= 2100) {
          for(i = 0; i < 10; i ++) {
            int life = therocket->lifetime;
            float old_x = (therocket->subrockets[i])->x;
            float old_y = (therocket->subrockets[i])->y;
            float angle = (therocket->subrockets[i])->angle;
            float new_x = old_x + (float) cos(angle) * 0.0003;
            float new_y = old_y + (((float) sin(angle) * 1 - gravity)/life);
            (therocket->subrockets[i])->x = new_x;
            (therocket->subrockets[i])->y = new_y;
            draw_spot(therocket->x + new_x, therocket->y + new_y);
            therocket->lifetime ++;
          }
        } else {
          draw_spot(therocket->x, therocket->y);
          therocket->y += 0.0004;
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
