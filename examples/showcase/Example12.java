public class Example2 {
    static void func1() {
        int rank = mpi.MPI.COMM_WORLD.Rank();
        int x = 6;
        if (rank == 0) {
            x++;
        } else if (rank == 1) {
            x--;
        }
    }
}