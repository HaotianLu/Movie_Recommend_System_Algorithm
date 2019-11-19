#Creates training set to then be fitted to linear regrsesion with no intercept
def knn_train_set(P,Q, train_ratings, b_u, b_i, global_b, k)
    
    #Create full 
    R_temp = (train_ratings.pivot(index='movieId', columns='userId', values='rating')).fillna(-1).values

    #Create mapping of movieId to movie index
    movie_unique =train_ratings.movieId.unique()
    movie_sorted_unique = np.sort(movie_unique)
    
    movie_value_index_mapping = dict(zip(movie_sorted_unique,range(len(movie_sorted_unique))))
    movie_index_value_mapping = {v: k for k, v in movie_value_index_mapping.items()}

    #Create mapping of userID to user index
    user_unique =train_ratings.userId.unique()
    user_sorted_unique = np.sort(user_unique)
    
    user_value_index_mapping = dict(zip(user_sorted_unique,range(len(user_sorted_unique))))
    user_index_value_mapping = {v: k for k, v in user_value_index_mapping.items()}

    #Find movie similarity matrix
    cosine_similar = cosine_similarity(Q)

    regression_train_x =[]
    regression_train_y = []

    for i in train_ratings.values:
        user = int(i[0])
        movie = int(i[1])
        individual_rating = float(i[2])

        movie_index = movie_value_index_mapping[movie]
        user_index = user_value_index_mapping[user]

        dot = np.dot(P[user_index],Q[movie_index].T)
            
        similarities = cosine_similar[movie_index]
            
        sorted_similarity= np.argsort(similarities)

        this_k = 0
        sum_ratings = 0

        for i in sorted_similarity:
            if R_temp[i][user_index] > 0:
                sum_ratings += R_temp[i][user_index]
                this_k+=1
            
            if this_k == k:
                break

        avg_similar_rating = 0
        if this_k > 0:    
            avg_similar_rating = float(sum_ratings/this_k)

        temp_b_u = b_u[user_index]
        temp_b_i = b_i[movie_index]

        x_train_values = [dot, temp_b_u, temp_b_i, global_b, avg_similar_rating ]

        regression_train_x.append(x_train_values)
        regression_train_y.append([individual_rating])

    return regression_train_x, regression_train_y