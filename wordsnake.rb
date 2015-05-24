# I created a program to find the longest word snake (the following word's
# starting letter is the previous word's last letter) in a given set of words.
# Output: swag, greta, apple. Inject in ruby works the
# same way as fold in Haskell.

def fixCombinations( array )
  array.inject([]) {
    |memo,word| memo << array.permutation(memo.length+1).to_a }.inject([]) {
    |mem,wor| mem + wor }
end

def isWordSnake( words )
  words.inject() {
    |memo,word| memo = memo[-1,1] != word[0,1] ? "" : word } == words[-1,1].to_s
end

def getLongestSnake( words )
  fixCombinations( words ).find_all{ |word| isWordSnake(word) }.max{
    |a,b| a.length <=> b.length }
end

puts getLongestSnake( ["greta",
                       "swag",
                       "volkswagen",
                       "toyota",
                       "vvv",
                       "apple"] )
