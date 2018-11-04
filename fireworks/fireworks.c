/* Ask for an OpenGL Core Context */

#define GLFW_INCLUDE_GLCOREARB
#define GLFW_INCLUDE_GLU

#include <GLFW/glfw3.h>
#include <stdlib.h>

#define BUFFER_OFFSET(i) ((char *)NULL + (i))
float hoj = 0.004f;

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
  float speed;
};

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
  }
  return raket;
}

struct rocket *therocket = NULL;

/* Draw a little diamond at a certain coordinate */
void draw_spot(float x, float y) {
  glBegin(GL_TRIANGLES);
  glColor3f(1.f, 1.f, 1.f);
  glVertex3f(x, y-hoj, 1);
  /* glColor3f(0.f, 1.f, 0.f); */
  glVertex3f(x + hoj/1.5, y, 1);
  /* glColor3f(0.f, 0.f, 1.f); */
  glVertex3f(x, y + hoj, 1);
  /* glColor3f(1.f, 1.f, 1.f); */
  glVertex3f(x, y+hoj, 1);
  glVertex3f(x-hoj/1.5, y, 1);
  glVertex3f(x, y-hoj, 1);
  glEnd();
}

static void key_callback(GLFWwindow *window,
                         int key,
                         int scancode,
                         int action,
                         int mods) {
  if (key == GLFW_KEY_ESCAPE && action == GLFW_PRESS) {
    therocket = new_rocket();
  }
}

int main(int argc, char** argv) {

  GLFWwindow* window;
  int xpos = -1, ypos;
  int i = 0;
  float hej = 0.0f;
  struct rocket raketen;

  raketen.x = 0.0f;
  raketen.y = -0.75f;

  /* Initialize the library */
  if ( !glfwInit() ) {
     return -1;
  }

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
    glfwGetFramebufferSize(window, &width, &height);
    glViewport(0, 0, width, height);
    glClear(GL_COLOR_BUFFER_BIT);

    if(therocket == NULL) {

    } else {
      draw_spot(therocket->x, therocket->y);
      if(therocket->lifetime >= 2100) {
        for(i = 0; i < 10; i ++) {
          (therocket->subrockets[i])->y += (therocket->subrockets[i])->speed;
          draw_spot(therocket->x + (therocket->subrockets[i])->x,
                    therocket->y + (therocket->subrockets[i])->y);
        }
      } else {
        therocket->y += 0.0004;
        therocket->lifetime ++;
      }
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
  return 0;
}
