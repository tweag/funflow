import argparse
import logging
import os
from typing import Tuple

import tensorflow as tf
import tensorflow_datasets as tfds
from tensorflow.keras import regularizers

logging.basicConfig()
tf.random.set_seed(42)


def normalize_img(image, label):
    """Normalizes images: `uint8` -> `float32`."""
    return tf.cast(image, tf.float32) / 255.0, label


def load_mnist(batch_size: int = 128) -> Tuple[tf.data.Dataset, tf.data.Dataset]:
    logging.info("Loading MNIST digits dataset")
    (ds_train, ds_test), ds_info = tfds.load(
        "mnist",
        split=["train", "test"],
        shuffle_files=True,
        as_supervised=True,
        with_info=True,
    )
    ds_train = ds_train.map(
        normalize_img, num_parallel_calls=tf.data.experimental.AUTOTUNE
    )
    ds_train = ds_train.cache()
    ds_train = ds_train.shuffle(ds_info.splits["train"].num_examples)
    ds_train = ds_train.batch(batch_size)
    ds_train = ds_train.prefetch(tf.data.experimental.AUTOTUNE)

    ds_test = ds_test.map(
        normalize_img, num_parallel_calls=tf.data.experimental.AUTOTUNE
    )
    ds_test = ds_test.batch(batch_size)
    ds_test = ds_test.cache()
    ds_test = ds_test.prefetch(tf.data.experimental.AUTOTUNE)
    return ds_train, ds_test


def normalize_img(image, label):
    """Normalizes images: `uint8` -> `float32`."""
    return tf.cast(image, tf.float32) / 255.0, label


def create_model() -> tf.keras.Model:
    logging.info("Initializing model")
    # Just using a simple CNN for this example
    model = tf.keras.models.Sequential(
        [
            tf.keras.layers.Conv2D(32, (3, 3), activation='relu', kernel_initializer="he_uniform", input_shape=(28, 28, 1)),
            tf.keras.layers.MaxPooling2D((2, 2)),
            tf.keras.layers.Conv2D(64, (3, 3), activation='relu', kernel_initializer="he_uniform"),
            tf.keras.layers.Conv2D(64, (3, 3), activation='relu', kernel_initializer="he_uniform"),
            tf.keras.layers.MaxPooling2D((2, 2)),
            tf.keras.layers.Dropout(0.5),
            tf.keras.layers.Flatten(),
            tf.keras.layers.Dense(
                100, activation="relu", kernel_regularizer=regularizers.l2(0.02)
            ),
            tf.keras.layers.Dense(10, activation="softmax"),
        ]
    )
    model.compile(
        loss="sparse_categorical_crossentropy",
        optimizer=tf.keras.optimizers.Adam(0.001),
        metrics=["accuracy"],
    )
    return model


def main(save_path: str, epochs: int) -> None:
    test, train = load_mnist()
    model = create_model()
    model.fit(train, validation_data=test, epochs=epochs)
    if not os.path.exists(save_path):
        os.makedirs(save_path)
    logging.info(f"Saving trained model to {save_path}")
    model.save(save_path)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "Trains an MNIST digit recognition model and saves it to the specified path"
    )
    parser.add_argument("model_dir", help="The path at which to save the model")
    parser.add_argument("--n_epochs", required=False, default=25, type=int)
    args = parser.parse_args()
    main(args.model_dir, args.n_epochs)
