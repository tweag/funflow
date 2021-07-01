import argparse
import logging
import os
from typing import List, Tuple

import matplotlib.pyplot as plt
import numpy as np
import tensorflow as tf
import tensorflow_datasets as tfds
from tensorflow.keras import regularizers

logging.basicConfig()
tf.random.set_seed(42)

# Image files and their correct labels
IMAGES = {"test-digit-1.png": 1, "test-digit-2.png": 2, "test-digit-3.png": 3}


def normalize_img(image: tf.Tensor) -> tf.Tensor:
    """Normalizes images: `uint8` -> `float32`."""
    return tf.cast(image, tf.float32) / 255.0


def load_image(image_path: str) -> tf.Tensor:
    logging.info(f"Loading image into tensor: {image_path}")
    with open(image_path, "rb") as f:
        image = tf.io.decode_image(
            f.read(),
            channels=1,
            dtype=tf.dtypes.uint8,
            name=None,
            expand_animations=True,
        )
    resized = 255 - tf.image.resize(image, [28, 28])
    normed = normalize_img(resized)
    return tf.expand_dims(normed, 0)


def load_all_images(res_dir: str, images: dict) -> Tuple[tf.Tensor, List]:
    tensors = []
    labels = []
    for img, label in images.items():
        tensors.append(load_image(os.path.join(res_dir, img)))
        labels.append(label)
    return tf.concat(tensors, axis=0), labels


def plot_predictions(
    images: np.ndarray, predictions: np.ndarray, out_path: str
) -> None:
    logging.info("Plotting predictions")
    fig = plt.figure()
    for i in range(images.shape[0]):
        plt.subplot(1, images.shape[0], i + 1)
        plt.imshow(
            images[i,]
        )
        plt.title(f"Predicted Digit: {predictions[i]}")
    logging.info(f"Saving figure to {out_path}")
    plt.savefig(out_path)


def main(model_path: str, res_path: str, out_path: str) -> None:
    logging.info(f"Loading saved model {model_path}")
    model = tf.keras.models.load_model(model_path)
    images, _ = load_all_images(res_path, IMAGES)
    logging.info("Running inferencing")
    predictions = np.argmax(model.predict(images), axis=-1)
    plot_predictions(images, predictions, out_path)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        "Loads a serialized keras model from disk and runs inferencing on a pre-defined set of images"
    )
    parser.add_argument("model_dir", help="The model directory to load")
    parser.add_argument("res_dir", help="The directory containing validation images")
    parser.add_argument("out_file", help="Output summary file path")
    args = parser.parse_args()
    main(args.model_dir, args.res_dir, args.out_file)
