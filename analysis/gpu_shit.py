import numpy as np
import tensorflow as tf
import sklearn.metrics

class Lasso(object):

    def __init__(self, alpha, fit_intercept):
        self.alpha = alpha
        self.fit_intercept = fit_intercept  # TODO: Implement support for intercept

    def score(self, A, b):
        return sklearn.metrics.r2_score(b, np.matmul(A, self.coef_))

    def fit(self, A_, b_):
        assert len(b_.shape) == 1
        assert b_.shape[0] == A_.shape[0]
        A = tf.constant(A_.astype(np.float32))
        b = tf.constant(b_.astype(np.float32))
        
        # make results reproducible
        seed = 13
        np.random.seed(seed)
        tf.set_random_seed(seed)
        
        # Create variables for linear regression
        x = tf.Variable(tf.random_normal(shape=[A_.shape[1]]))
        
        # Declare model operations
        b_hat = tf.squeeze(tf.matmul(A, tf.expand_dims(x,1)))
        
        # Declare Lasso loss function
        # Lasso Loss = L2_Loss + heavyside_step,
        # Where heavyside_step ~ 0 if A < constant, otherwise ~ 99
        alpha = tf.constant(self.alpha)
        loss = tf.add(
                0.5 * tf.reduce_mean(tf.square(b - b_hat)),
                tf.reduce_sum(alpha * tf.abs(x)))
    
        opt = tf.train.AdamOptimizer(0.01)
        train_step = opt.minimize(loss)

        config = tf.ConfigProto()
        config.gpu_options.allow_growth=True

        with tf.Session(config=config) as sess:
            init = tf.global_variables_initializer()
            sess.run(init)
            for i in range(3000):
                _, this_loss = sess.run([train_step, loss])
                if i == 0 or (i+1) % 300 ==0:
                    print('Step %d Loss = %s' % (i + 1, str(this_loss)))
                if this_loss < 0.000001:
                    break
            self.coef_ = sess.run(x)
