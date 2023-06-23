import mpi.MPI;
public class HelloMPJ {
  public static void main(String[] args) {
    MPI.Init(args);
    int size = MPI.COMM_WORLD.Size();
    int rank = MPI.COMM_WORLD.Rank();
    System.out.printf("Process %d of %d\n",
            rank, size);
    MPI.Finalize();
  }
}

