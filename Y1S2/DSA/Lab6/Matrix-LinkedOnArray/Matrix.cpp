#include <exception>
#include <stdexcept>
#include "Matrix.h"

Matrix::Matrix(int nrLines, int nrCols) : lines{nrLines}, columns{nrCols} {
  capacity = 8;
  firstEmpty = 0;
  root = -1;
  array = new Node[8];

  // Initialize all nodes to be empty.
  for (int i = 0; i < 8; ++i)
    array[i] = {NULL_TRIPLE, -1, i + 1};

  array[7].right = -1;
}

Matrix::~Matrix() {
  delete[] array;
}

int Matrix::nrLines() const {
  return lines;
}

int Matrix::nrColumns() const {
  return columns;
}

TElem Matrix::element(int i, int j) const {
  if (i < 0 || i >= lines)
    throw std::runtime_error{"Matrix::element: i is out of range."};

  if (j < 0 || j >= columns)
    throw std::runtime_error{"Matrix::element: j is out of range."};

  auto triple = Triple{i, j, NULL_TELEM};
  auto node = root;

  // Go down the tree until we either reach the end, or we find
  // the element we were loooking for.
  while (node != -1 && array[node].triple != triple)
    if (array[node].triple < triple)
      node = array[node].right;
    else
      node = array[node].left;

  // If we didn't find the element
  if (node == -1) {
    return NULL_TELEM;
  } else { // If we found the element
    return array[node].triple.value;
  }
}

void Matrix::resize() {
  auto oldCapacity = capacity;
  auto oldArray = array;

  // Realloc array with bigger size.
  capacity *= 2;
  array = new Node[capacity];

  // Copy old elements into new array.
  for (int i = 0; i < oldCapacity; ++i)
    array[i] = oldArray[i];

  // Initialize the rest of the new array with empty nodes.
  for (int i = oldCapacity; i < capacity; ++i)
    array[i] = {NULL_TRIPLE, -1, i + 1};

  array[capacity - 1].right = -1;
  firstEmpty = oldCapacity;

  delete[] oldArray;
}

// Allocate a new node from the available free nodes, and
// resize the array if there are no free nodes.
int Matrix::allocate() {
  if (firstEmpty == -1)
    resize();

  auto node = firstEmpty;
  firstEmpty = array[firstEmpty].right;

  array[node] = {NULL_TRIPLE, -1, -1};
  return node;
}

// Deallocate a node, emptying its data and setting it as
// the new firstEmpty.
void Matrix::deallocate(int node) {
  array[node] = {NULL_TRIPLE, -1, firstEmpty};
  firstEmpty = node;
}

// Check if the triple can be inserted after the node.
bool Matrix::canInsertAfter(int node, Triple triple) {
  return
    array[node].triple == triple
    || (array[node].triple < triple && array[node].right == -1)
    || (array[node].triple > triple && array[node].left == -1);
}

TElem Matrix::add(int i, int j, TElem e) {
  auto triple = Triple{i, j, NULL_TELEM};

  // If the tree is empty, simply set the root to the value.
  if (root == -1) {
    root = allocate();
    array[root].triple = {i, j, e};
    return NULL_TELEM;
  }

  auto node = root;

  // Go down the tree until we find a node we can insert after.
  while (!canInsertAfter(node, triple)) {
    if (array[node].triple < triple)
      node = array[node].right;
    else
      node = array[node].left;
  }

  // If we found a node with the same key, we need to replace the value.
  if (array[node].triple == triple) {
    auto oldV = array[node].triple.value;
    array[node].triple.value = e;

    return oldV;
  } else { // If we didn't find a node with the same key, we need to allocate a node and add it to the tree.
    auto newNode = allocate();
    array[newNode].triple = {i, j, e};

    if (array[node].triple < triple)
      array[node].right = newNode;
    else
      array[node].left = newNode;

    return NULL_TELEM;
  }
}

// Finds the parent of the maximum node of the left subtree of the specified node.
int Matrix::findParentOfMaximum(int original) {
  if (original == -1 || array[original].left == -1)
    return -1;

  auto startingNode = array[original].left;

  if (startingNode == -1)
    return -1;

  // If the left subtree has exactly 1 node
  if (array[startingNode].left == -1 && array[startingNode].right == -1)
    return original;

  if (startingNode == -1 || array[startingNode].right == -1)
    return -1;

  auto parent = startingNode;
  auto prev = array[startingNode].right;
  auto node = array[prev].right;

  // Go down the tree until you reach the end.
  while (node != -1) {
    parent = prev;
    prev = node;
    node = array[node].right;
  }

  return parent;
}

TElem Matrix::remove(int i, int j) {
  auto triple = Triple{i, j, NULL_TELEM};

  if (root == -1) 
    return NULL_TELEM;

  // If the element we're looking for is the root itself.
  if (array[root].triple == triple) {
    auto oldV = array[root].triple.value;

    // If the root has no children i.e. the tree has exactly 1 node.
    if (array[root].left == -1 && array[root].right == -1) {
      deallocate(root);
      root = -1;
    } else if (array[root].left == -1) { // Else if the root has only a right child.
      auto oldRoot = root;
      root = array[root].right;
      deallocate(oldRoot);
    } else if (array[root].right == -1) { // Else if the root has only a left child.
      auto oldRoot = root;
      root = array[root].left;
      deallocate(oldRoot);
    } else { // If the root has 2 children, we need to find the maximum of the left subtree.
      auto parentMax = findParentOfMaximum(root);

      int max;

      // If the left subtree has exactly 1 node.
      if (parentMax != root) {
        max = array[parentMax].right;
        array[parentMax].right = -1;
      } else {
        max = array[parentMax].left;
        array[parentMax].left = -1;
      }

      array[root].triple = array[max].triple;
      deallocate(max);
    }

    return oldV;
  }

  auto prev = root;
  auto dir = (array[root].triple < triple);
  int node;

  if (dir)
    node = array[root].right;
  else
    node = array[root].left;

  // Go down the tree until either we reach the end, or we find the element we need.
  while (node != -1 && array[node].triple != triple) {
    prev = node;
    dir = (array[node].triple < triple);

    if (dir)
      node = array[node].right;
    else
      node = array[node].left;
  }

  // If we reached the end, then the element does not exist.
  if (node == -1)
    return NULL_TELEM;

  auto oldV = array[node].triple.value;

  // If we found the element, we have to remove it.

  // If the node has no children i.e. the tree has exactly 1 node.
  if (array[node].left == -1 && array[node].right == -1) {
    if (dir)
      array[prev].right = -1;
    else
      array[prev].left = -1;

    deallocate(node);
  } else if (array[node].left == -1) { // Else if the root has only a right child.
    if (dir)
      array[prev].right = array[node].right;
    else
      array[prev].left = array[node].right;

    deallocate(node);
  } else if (array[node].right == -1) { // Else if the root has only a left child.
    if (dir)
      array[prev].right = array[node].left;
    else
      array[prev].left = array[node].left;

    deallocate(node);
  } else { // If the root has 2 children, we need to find the maximum of the left subtree.
    auto parentMax = findParentOfMaximum(node);

    int max;

    // If the left subtree has exactly 1 node.
    if (parentMax != node) {
      max = array[parentMax].right;
      array[parentMax].right = -1;
    } else {
      max = array[parentMax].left;
      array[parentMax].left = -1;
    }

    array[node].triple = array[max].triple;
    deallocate(max);
  }

  return oldV;
}

TElem Matrix::modify(int i, int j, TElem e) {
  if (i < 0 || i >= lines)
    throw std::runtime_error{"Matrix::modify: i is out of range."};

  if (j < 0 || j >= columns)
    throw std::runtime_error{"Matrix::modify: j is out of range."};

  // If the element is 0, then modify needs to delete it from the matrix.
  // Otherwise, modify adds the element to the matrix.
  if (e == 0)
    return remove(i, j);
  else
    return add(i, j, e);
}

int Matrix::numberOfNonZeroElems(int col) const {
  if (col < 0 || col >= nrColumns())
    throw std::runtime_error{"Matrix::numberOfNonZeroElems: Invalid column specified."};
    
  int number = 0;

  for (int i = 0; i < nrLines(); ++i) {
    if (element(i, col) != 0)
      ++number;
  }

  return number;
}