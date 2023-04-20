import numpy as np
import numpy.typing as npt
from typing import Optional, List
from scipy.spatial.distance import cdist


class SSFCM:
    def __init__(
        self,
        n_clusters: int,
        max_iter: Optional[int] = 200,
        m: Optional[float] = 2.0,
        U: Optional[npt.ArrayLike] = None,
        error: Optional[float] = 1e-4,
        random_state: Optional[int] = 2022,
        v_with_alpha: Optional[bool] = True
    ) -> None:
        self.n_clusters = n_clusters
        self.max_iter = max_iter
        self.m = m
        self.U = U
        self.error = error
        self.random_state = random_state
        self.v_with_alpha = v_with_alpha
        self.iterations: int = 0
        self.errors: List[int] = [None]*self.max_iter
        self.cluster_centers: npt.ArrayLike = None
        self.alpha: Optional[float] = None
        self.us: List = [None]*self.max_iter
        self.vs: List = [None]*self.max_iter
    
    def fit(
        self,
        X: npt.ArrayLike,
        alpha: Optional[float] = None,
        F: Optional[npt.ArrayLike] = None
    ) -> None:
        self._rng = np.random.default_rng(self.random_state)
        self._n_samples = X.shape[0]
        
        if F is None:
            raise Exception("Regular FCM not implemented yet!")
        else:
            i_indices = F.sum(axis=1).nonzero()[0]
            j_indices = np.arange(0, F.shape[0])
            h_indices = np.setdiff1d(j_indices, i_indices)
            self.i_indices = i_indices
            self.j_indices = j_indices
            self.h_indices = h_indices
        
        # l = 0
        if self.U is None:
            self.U = self._rng.uniform(size=(self._n_samples, self.n_clusters))
            self.U = self.U / np.tile(self.U.sum(axis=1)[np.newaxis].T, self.n_clusters)
        else:
            assert sum(self.U.sum(axis=1)) == float(self.U.shape[0]), "Values do not sum up to 1!"
    
        for iteration in range(self.max_iter):
            self.iterations += 1

            U_previous_l = self.U.copy()
            self.us[iteration] = U_previous_l

            self.cluster_centers = self.update_cluster_centers(
                alpha,
                U_previous_l,
                F,
                h_indices,
                X
            )

            self.vs[iteration] = self.cluster_centers

            self.U = self.update_memberships(
                X,
                self.cluster_centers,
                F,
                i_indices,
                alpha
            )

            self.errors[iteration] = np.linalg.norm(self.U - U_previous_l)
            if self.errors[iteration] < self.error:
                break

    def update_cluster_centers(
        self,
        alpha,
        U,
        F,
        h_indices,
        X
    ):
        UF = alpha * (U-F)**self.m
        UF[h_indices] = 0.
        Phi = U**self.m + UF

        # V = (X.T @ Phi).T
        if self.v_with_alpha:
            V = (X.T @ Phi / np.sum(Phi, axis=0)).T
        else:
            V = (X.T @ U**self.m / np.sum(U**self.m, axis=0)).T

        return V

    def update_memberships(
        self,
        X,
        V,
        F,
        i_indices,
        alpha
    ):
        # distances matrix D
        D = cdist(X, V, metric='Euclidean')**2

        # evidence matrix E
        #D_reshaped = D[:, np.newaxis, :]
        D_reshaped = D.reshape(self._n_samples, 1, -1)
        D_repeated = D_reshaped.repeat(D.shape[-1], axis=1)

        D_newaxis = D[:, :, np.newaxis]

        E = 1 / ( (D_newaxis / D_repeated).sum(axis=2) )

        # multiplier matrix M
        M = np.ones(F.shape)
        M[i_indices, :] = 1 / (1+alpha)

        # ALB matrix
        ALB = F * ( alpha / (1+alpha) )

        # final memberships matrix
        U = np.multiply(M, E) + ALB

        return U