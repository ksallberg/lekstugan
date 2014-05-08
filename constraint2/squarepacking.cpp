#include <gecode/int.hh>
#include <gecode/gist.hh>
#include <gecode/minimodel.hh>
#include <stdio.h>
#include <math.h>

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

      const static int n = 6;

      // Two intvarArrays which give the respective x and y
      // coordinates for each square to be packed.
      IntVarArray x;
      IntVarArray y;
      
   public:
      SquarePacking(void) {
        
         // first we calculate the maximum possible outer size
         // by placing all squares next to each other
         int maxS = 0;
         int minSpaceSize = 0;
         for(int i = 0; i < n; i ++) {
            maxS += size(i);
            minSpaceSize += size(i)*size(i);
         }
	
         // assign s to be between 0 and s.max()
         s = IntVar     (*this,ceil(sqrt(minSpaceSize)), maxS);
         x = IntVarArray(*this,n,0,maxS);
         y = IntVarArray(*this,n,0,maxS);
         
      // ___________________________________ part 2:

         for(int square=0; square<n; square++){
		 rel(*this, x[square]+size(square)<= s);
		 rel(*this, y[square]+size(square)<= s);
	 }
         // express with reification that no two squares overlap. Two squares s1
         // and s2 do not overlap iff

         // s1 is left of s2 OR
         // s2 is left of s1 OR
         // s1 is above s2   OR
         // s2 is above s1
         for(int i = 0; i < n; i ++) {

            for(int j = i+1; j < n; j ++) {
               
               IntVar a(*this,0,2*s.max());
               IntVar b(*this,0,2*s.max());
               IntVar c(*this,0,2*s.max());
               IntVar d(*this,0,2*s.max());
               
               BoolVarArgs boo(*this,4,0,1);

               rel(*this, a == x[i]+size(i));
               rel(*this, a, IRT_LQ, x[j], boo[0]);

               rel(*this, b == x[j]+size(j));
               rel(*this, b, IRT_LQ, x[i], boo[1]);

               rel(*this, c == y[i]+size(i));
               rel(*this, c, IRT_LQ, y[j], boo[2]);

               rel(*this, d == y[j]+size(j));
               rel(*this, d, IRT_LQ, y[i], boo[3]);
  


             
	       linear(*this, boo, IRT_GQ, 1);
            }
         }

      // ________________________________________ part 3:
         IntArgs allSizes = IntArgs(n);
	 for(int square =0; square<n;square++){
		allSizes[square]= size(square);
	 }
	 for(int i = 0; i<s.max(); i++){
		BoolVarArgs reifCol(*this,n,0,1);
		BoolVarArgs reifRow(*this,n,0,1);		
		for(int square=0; square<n;square++){
			dom(*this,x[square],i,i+size(square)-1,reifCol[square] );
			dom(*this,y[square],i,i+size(square)-1,reifRow[square]);
		}
		linear(*this,allSizes,reifCol, IRT_LQ,s);
		linear(*this,allSizes,reifRow, IRT_LQ,s);
	 }


         branch(*this, s, INT_VAL_MIN());

	 branch(*this, y, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
         branch(*this, x, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
      }
      
      // update all different constructs
      SquarePacking(bool share, SquarePacking& sp) : Space(share, sp) {
         
         // update the IntVarArray
         x.update(*this, share, sp.x);
         y.update(*this, share, sp.y);
	 s.update(*this, share, sp.s);
      }
      
      virtual Space* copy(bool share) {
         
         return new SquarePacking(share,*this);
      }
      
      // return the size of the square with number i
      // i should take the value of 0 -> n
      static int size( int i) {
         
         return n-i;
      }
      
      // print the sudoku solution
      void print(void) const {
         
        printf("Solution found for N=%d\n",n);
	printf("Minimum s=%d\n", s.val()); 
	 
	for(int i = 0; i < n; i++) {
		printf("Square (%d x %d) placed at (%d,%d)\n", size(i),size(i),x[i].val(),y[i].val(),s.val()); 
        }
      }
};

int main(int argc, char* argv[]) {
   SquarePacking* squarePacking = new SquarePacking;
   DFS<SquarePacking> e(squarePacking);
   SquarePacking* s = e.next();
   //while (SquarePacking* s = e.next()) {
      s->print();
      delete s;
   //}

   //Gist::dfs(squarePacking);
   delete squarePacking;
   return 0;
}
