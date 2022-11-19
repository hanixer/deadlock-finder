
package mpi.corrbench;
import mpi.MPI;

public class Ranks {
  public static void main(String[] args) {
    int rank = MPI.COMM_WORLD.Rank();
    int x;
    if (rank == 0) {
      if (x == 3) {
        x = 11;
      } else {
        x = 22;
      }
    } else if (rank == 1) {
      x = 33;
    }
  }
}
