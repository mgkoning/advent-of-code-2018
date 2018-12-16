namespace AdventOfCode2018.Day09 {
  public class Node<T> {
    public static Node<T> Singleton(T value) {
      var node = new Node<T>(value);
      node.Next = node;
      node.Previous = node;
      return node;
    }

    private Node(T value) {
      Value = value;
    }

    public T Value { get; }
    public Node<T> Next { get; private set; }
    public Node<T> Previous { get; private set; }
    public Node<T> InsertAfter(T value) {
      var newNode = new Node<T>(value);
      Next.Previous = newNode;
      newNode.Next = Next;
      newNode.Previous = this;
      Next = newNode;
      return newNode;
    }

    public void Delete() {
      Next.Previous = Previous;
      Previous.Next = Next;
      Next = Previous = null;
    }
  }
}
