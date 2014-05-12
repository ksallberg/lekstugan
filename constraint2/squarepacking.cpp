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

      const static int n = 10;

      // Two intvarArrays which give the respective x and y
      // coordinates for each square to be packed.
      IntVarArray x;
      IntVarArray y;
      
   public:
      SquarePacking(void) {
        
         // first we calculate the maximum possible outer size
         // by placing all squares next to each other and add their size
         int maxS = 0;
         for(int i = 0; i < n; i ++) {
            maxS += size(i);
         }

	 int minSpaceSize = (n*(n+1)*(2*n+1))/6;
	
         // assign s bounds.  
         s = IntVar     (*this,ceil(sqrt(minSpaceSize)), maxS);
         x = IntVarArray(*this,n,0,maxS);
         y = IntVarArray(*this,n,0,maxS);
         
      // PART 2:

         for(int square=0; square<n-1; square++){
		 rel(*this, x[square]+size(square)<= s);
		 rel(*this, y[square]+size(square)<= s);
	 }
         // Express with reification that no two squares overlap. Two squares s1
         // and s2 do not overlap iff

         // s1 is left of s2 OR
         // s2 is left of s1 OR
         // s1 is above s2   OR
         // s2 is above s1
         for(int i = 0; i < n-1; i ++) {

            for(int j = i+1; j < n-1; j ++) {
               
              IntVar a(*this,0,s.max()+n);
              IntVar b(*this,0,s.max()+n);
              IntVar c(*this,0,s.max()+n);
              IntVar d(*this,0,s.max()+n);
              
	      //Boolean Reification Variables 
              BoolVarArgs brv(*this,4,0,1);

                // s1 is left of s2
               rel(*this, a == x[i]+size(i));
               rel(*this, a, IRT_LQ, x[j], brv[0]);
	
         	// s2 is left of s1
               rel(*this, b == x[j]+size(j));
               rel(*this, b, IRT_LQ, x[i], brv[1]);

         	// s1 is above s2
               rel(*this, c == y[i]+size(i));
               rel(*this, c, IRT_LQ, y[j], brv[2]);

         	// s2 is above s1
               rel(*this, d == y[j]+size(j));
               rel(*this, d, IRT_LQ, y[i], brv[3]);
	       
		//At least 1 of the above constraints should be true
		linear(*this, brv, IRT_GQ, 1);

            }
         }

      // PART 3:
         IntArgs allSizes = IntArgs(n);
	 for(int square =0; square<n;square++){
		allSizes[square]= size(square);
	 }
	 for(int i = 0; i<s.max(); i++){
		BoolVarArgs reifCol(*this,n,0,1);
		BoolVarArgs reifRow(*this,n,0,1);		
		for(int square=0; square<n-1;square++){
			dom(*this,x[square],i,i+size(square)-1,reifCol[square] );
			dom(*this,y[square],i,i+size(square)-1,reifRow[square]);
		}
		linear(*this,allSizes,reifCol, IRT_LQ,s);
		linear(*this,allSizes,reifRow, IRT_LQ,s);
	 }

      	// PART 4
      	// (a) By having as lowest bound for s: min(s) = ceil(sqrt(n*(n+1)(2*n+1)/6))
      	// we guarrantee that the outer square formed by the lowest bound min(s) has enough space for 
      	// fitting the total sum of space size of all inner squares.

        //We tried to remove symmetries by applying the following constraint presented in the paper on Section 2.2
	// rel(*this,x[0]== 1 + (s-n)/2);
	// However, the above formula did not make sense and did not yield to an optimal solution.
	// Hence we just assign the largest square to a corner.
	rel(*this,x[0]==0);
	rel(*this,y[0]<= x[0]);

	//The above 2 rel constraints put the largest square to the upper left corner [0,0]

 
	//PART 5
	 //(a) Always branch on s first. For value selction we use the INT_VAL_MIN() strategy
	 //in order to guarrantee that the 1st solution that we find is an optimal one.
         branch(*this, s, INT_VAL_MIN());
	 
	 //(b) Try to assign all x-coordinates, then all y-coordinates
	 //(c)For variable selection we use the INT_VAR_NONE() strategy in order to pick
	 //the first unassigned variable which corresponds to the largest square
         branch(*this, x, INT_VAR_NONE(), INT_VAL_MIN());
	 branch(*this, y, INT_VAR_NONE(), INT_VAL_MIN());
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
	 
	for(int i = 0; i < n-1; i++) {
		printf("Square (%d x %d) placed at (%d,%d)\n", size(i),size(i),x[i].val(),y[i].val(),s.val()); 
        }
	printf("Square (1 x 1) is guarranteed to have available space from how min(s) is calculated.\n");
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

   Gist::dfs(squarePacking);
   delete squarePacking;
   return 0;
}
