/*
A javascript implementation of the Binary Search Tree. It's built in a
functional manner, where every operation builds a new tree for the affected
part of the tree, that probably makes it slow for large trees.

Usage looks like:
var t1 = new BinarySearchTree( 80 );
t1 = addNode(new BinarySearchTree(23), t1);
t1 = removeNode(23, t1);
*/

/**
*   Constructor, add whatever content should
*   be at the root
*/
function BinarySearchTree( cont ) {

    this.content = cont
    this.leftSub;
    this.rightSub;

    this.toString = function() {

        return preOrderTraverse( 0, this );
    }
}

/**
*   Add a node to a tree and get a newly constructed tree back
*/
function addNode( treetoAdd, tree ) {

    //If what we want to add is already in there,
    //just return the tree
    if( treetoAdd.content == tree.content ) {

        return tree;

    //If what we want to add is larger than the content at the
    //node we are currently at, go into the right sub tree
    } else if ( treetoAdd.content > tree.content ) {

        //If the right sub tree is not empty, then go into it
        if( tree.rightSub != null ) {

            tree.rightSub = addNode( treetoAdd, tree.rightSub );

        //If it's empty, that's the spot where toAdd should
        //be added!
        } else {

            tree.rightSub = treetoAdd;
        }

    //If what we want to add is smaller than the content at the
    //node we are at
    } else {

        //If the left sub tree is not empty, go there!
        if( tree.leftSub != null ) {

            tree.leftSub = addNode( treetoAdd, tree.leftSub )

        //Else, add toAdd into that empty spot
        } else {

            tree.leftSub = treetoAdd;
        }
    }

    //all the returns from the above tree modifications here
    return tree;
}

/**
*   Remove a node in a tree and get a newly constructed tree back
*/
function removeNode( toRemove, tree ) {

    //If what you want to remove is the node we are at right now
    if( toRemove == tree.content ) {

        //If both sub trees are null, just set the node to null
        if( tree.leftSub == null && tree.rightSub == null )
            return null;

        //return newRightSu
        if( tree.rightSub != null && tree.leftSub != null )
            return addNode( tree.leftSub, tree.rightSub );

        //If just the left sub tree is null, remove this node and
        //replace it with the right sub tree
        if( tree.leftSub == null )
            return tree.rightSub

        //If just the right sub is null, remove this node and
        //replace it with the left sub tree
        if( tree.rightSub == null )
            return tree.leftSub

        return tree;

    //The element to remove is in the right sub tree
    } else if ( toRemove > tree.content ) {

        //If we have a right sub tree, try to remove the element
        //from there
        if( tree.rightSub != null )
            tree.rightSub = removeNode( toRemove, tree.rightSub );

    //The element to remove is in the left sub tree
    } else {

        //If the element is in the right
        if( tree.leftSub != null )
            tree.leftSub = removeNode( toRemove, tree.leftSub );
    }

    //If the element isn't in the tree, also the returns from 2
    //if cases above
    return tree;
}

/**
*   Search for an element, return null if it's not in the tree
*/
function get( elem, tree ) {

    //If the content we are looking for is in this node
    if( elem == tree.content ) {

        return tree.content;

    //If what we want to get is larger than the content at the
    //node we are currently at, go into the right sub tree
    } else if ( elem > tree.content ) {

        //If the right sub tree is not empty, then go into it
        if( tree.rightSub != null ) {

            return get( elem, tree.rightSub );

        //elem isn't in the tree
        } else {

            return null;
        }

    //If what we want to get is smaller than the content at the
    //node we are at
    } else {

        //If the left sub tree is not empty, go there!
        if( tree.leftSub != null ) {

            return get( elem, tree.leftSub )

        //elem isn't in the tree
        } else {

            return null;
        }
    }
}

/**
*   Traverse through the tree, pre order
*   ()
*/
function preOrderTraverse( levels, tree ) {

    var before = "<br>"

    for( i  = 0; i < levels; i ++)
        for( j  = 0; j < 12; j ++)
            before += "&nbsp;"

    if( tree == null )
        return before + "null"

    return  before + tree.content +
            preOrderTraverse( levels + 1, tree.leftSub ) +
            preOrderTraverse( levels + 1, tree.rightSub )
}
