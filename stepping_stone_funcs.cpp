#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// This function returns a valid neighbour index for each cell
int subm_index(int M, int x)
{
  if (x < 0)
    return 0;
  if(x >= M )
    return M-1;
  return x;
}

// [[Rcpp::export]]
arma::mat iterate_stepping(arma::mat M) {
  std::uniform_real_distribution<double> dist(0.0, 1.0);
  std::random_device rd;
  std::mt19937 mt(rd());

  int m = M.n_rows;
  int n = M.n_cols;
  
  // Moore neighborhood indexes to sample
  const char arrayNeighbors[8] = {'1', '2', '3', '4', '5', '6', '7', '8'};

  for(int i = 0; i < m; i++) {
    for(int j = 0; j < n; j++){
      // Pick a random Moore neighbor
      char Neighbor = arrayNeighbors[rand() % 8];
      // 1 = North
        if (Neighbor == '1')
        {
          int new_j = subm_index(n, j+1);
          // Ponderation to mix colours between cell and its neighbor
          double p = dist(mt); 
          M(i,j) = p*M(i, new_j) + (1-p)*M(i,j);
        }
        // 2 = South
        else if (Neighbor == '2')
        {
          int new_j = subm_index(n, j-1);
          double p = dist(mt);
          M(i,j) = p*M(i,new_j) + (1-p)*M(i,j);
        }
        // 3 = East
        else if (Neighbor == '3')
        {
          int new_i = subm_index(m, i+1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,j) + (1-p)*M(i,j);
        }
        // 4 = West
        else if (Neighbor == '4')
        {
          int new_i = subm_index(m, i-1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,j) + (1-p)*M(i,j);
        }
        // 5 = North-West
        else if (Neighbor == '5')
        {
          int new_i = subm_index(m, i-1);
          int new_j = subm_index(n, j+1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,new_j) + (1-p)*M(i,j);
        }
        // 6 = North-East
        else if (Neighbor == '6')
        {
          int new_i = subm_index(m, i+1);
          int new_j = subm_index(n, j+1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,new_j) + (1-p)*M(i,j);
        }
        // 7 = South-West
        else if (Neighbor == '7')
        {
          int new_i = subm_index(m, i-1);
          int new_j = subm_index(n, j-1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,new_j) + (1-p)*M(i,j);
        }
        // 8 = South-East
        else
        {
          int new_i = subm_index(m, i+1);
          int new_j = subm_index(n, j-1);
          double p = dist(mt);
          M(i,j) = p*M(new_i,new_j) + (1-p)*M(i,j);
        }
    }
  }
  return M;
}

