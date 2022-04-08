import mpi.MPI;

public class LoopRemaining {
    public static void main(String[] args) {
        MPI.Init(args);
        int rank = MPI.COMM_WORLD.Rank();

        int[] buf = new int[10];

        if (rank == 0) {
            int counter = 0;
            for (int i = 0; i < 2; i++) {
                MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 1, 0);
                counter++;
            }
            for (int j = 0; j < counter; j++) {
                MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 1, 0);
            }
        } else if (rank == 1) {
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Recv(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
            MPI.COMM_WORLD.Send(buf, 0, buf.length, MPI.INT, 0, 0);
        }
    }
}
