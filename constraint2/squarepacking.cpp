#include <gecode/int.hh>
#include <gecode/gist.hh>
#include <gecode/minimodel.hh>
#include <stdio.h>

/* 
 * 
 * Authors: Kristian Sällberg, Siskos Filotas
 * 
 */

using namespace Gecode;

/* 
* For a given n:
* Find an enclosing square of size s*s such that the n squares
* of size 1x1,2x2,...,nxn
*
* s should be minimal
*/
class SquarePacking : public Space {
   
   protected:
      // s = w = h
      IntVar s; // gives the size s of the surrounding square

      const static int n = 4;

      // Two intvarArrays which give the respective x and y
      // coordinates for each square to be packed.
      IntVarArray x;
      IntVarArray y;

   public:
      SquarePacking(void) {

         x = IntVarArray(*this,n,0,n);
         y = IntVarArray(*this,n,0,n);

         branch(*this, y, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
         branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
      }
         
      // update all different constructs
      SquarePacking(bool share, SquarePacking& s) : Space(share, s) {
         
         // update the IntVarArray
         x.update(*this, share, s.x);
         y.update(*this, share, s.y);
      }
      
      virtual Space* copy(bool share) {
         
         return new SquarePacking(share,*this);
      }
      
      // return the size of the square with number i
      // i should take the value of 0 -> n
      int size(int i) {
         
         return n - i;
      }

      // print the sudoku solution
      void print(void) const {
         
         std::cout << "Square packing!" << std::endl;

         for(int i = 0; i < n; i++) {
            std::cout << "HEJ" << std::endl;
            std::cout << x[i]  << std::endl;
            std::cout << y[i]  << std::endl;
         }
      }
};

int main(int argc, char* argv[]) {
   
   SquarePacking* squarePacking = new SquarePacking;
   DFS<SquarePacking> e(squarePacking);
   
   while (SquarePacking* s = e.next()) {
      s->print();
      delete s;
   }

   Gist::dfs(squarePacking);
   delete squarePacking;
   return 0;
}
