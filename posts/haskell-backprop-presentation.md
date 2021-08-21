---
title: "Quick and dirty backpropagation in Haskell (slides)"
date: 2021-06-02
published: false
---

This is the presentation version of [the blog post](/posts/haskell-backprop-short.html). You probably want to read the blog post instead. You can use `Page Up` and `Page Down` to switch slides. Also, see the blog post for diagrams credits.

***

## Quick and dirty backpropagation in Haskell

Francesco Mazzoli -- <code><a href="mailto:f@mazzo.li">f@mazzo.li</a></code> -- <code>@trascendentale</code> -- 2021-06-02 -- HaskellerZ

<div class="center-image">![backpropagation animation](/assets/images/mlp-animation.gif)</div>

<!--
Hi everybody, I'm very happy to be speaking at HaskellerZ today. I of course have been here before live, but this time I'm remote from Rome.

Andreas was kind enough to invite me after reading a blog post on this topic.

We're going to train a very simple network in Haskell. The network classifies points in two semicircles. This animation shows how the network learns where the first semicircle ends and the second begins. You can also find the full blog post on my website.
-->

***

## Quick and dirty backpropagation in Haskell

Goal: showcase all moving parts involved in training a neural network, without much code.

No previous neural-network knowledge assumed, very basic calculus knowledge assumed.

Code: <https://github.com/bitonic/micrograd-ad>.

Blog post: <https://mazzo.li/posts/haskell-backprop-short.html>.

<!--

The goal of the article and this talk is to showcase the basic ingredients involved in neural network training, with little effort.

The details are very different from the state of the art, but the core mechanisms are the same.

I'll assume no neural-network knowledge, and minimal calculus knowledge. Feel free to stop me at any point, I'm not sure how that works best on zoom.

-->

***

## Credits

* The sample network is taken from `micrograd`: <https://github.com/karpathy/micrograd>;
* The Haskell automatic differentiation is powered by `ad`: <https://github.com/ekmett/ad>.

<!--

Before we start -- the example networks is lifted entirely from micrograd, which is a project which also gives a minimal account of neural network training.

The core algorithm is provided by ad, which is a very neat Haskell library by Ed Kmett.

-->

***

## How to get there

<div class="center-image">![backpropagation animation](/assets/images/mlp-animation.gif)</div>

Steps:

* Multi-layer perceptrons in Haskell;
* Problem setup;
* Backpropagation and the `ad` library;
* Training

<!--

We will proceed as follows:

* First, we'll implement a minimal neural network data structure in Haskell, implementing a simple form of network called "multi-layer perceptron"
* Then, we'll go over the problem and how it can be phrased as a neural network
* We'll then explain backpropagation and how the `ad` library helps us
* And finally we'll put everything together by traininig our network to recognize our moons

-->
***

## Multi-layer perceptrons

$x_1, ..., x_n$ inputs go in, $y_1, ..., y_m$ outputs come out:

<div class="center-image" style="margin-top:1rem">
![](/assets/images/neural-network-diagram.svg)
</div>

* A **neuron** is a the smallest computational unit (the large circles above);
* A **layer** is a "column" of neurons;
* A **multi-layer perceptron** is a bunch of layers.

<!--

So, multi-layer perceptrons are some of the simplest neural-network around.

They do _not_ look like the ones you'd find in modern deep learning model (the biggest difference being in how the neurons are laid out and connected to one another), but they work well enough for our problem.

An MLP has a number of inputs (in the diagram above 3) and some number of output (in the diagram above 2). You feed it the inputs, some computation happens in the neurons, and some outputs come out.

The arrows above represent how data flows. Each neuron accepts multiple inputs *point at arrows in*, and produces a single output that "fans out" to multiple places. So for example n_{12} has 3 inputs, and 1 output that goes to 4 neurons in the next layer.

In fact, at each layer, each output fans out to all the neurons in the next layer, until the last layer, where the outputs of the neurons are the outputs of the overall network.

-->

***

## Neurons

Each neuron processes its inputs $x_1, \dots, x_n$ as follows:

$$\phi(b + x_1 w_1 + \dots + x_n w_n)$$

$w_1, ..., w_n$ are the _weights_.

$b$ is the _bias_.

$\phi$ is the _activation function_, it adds some flavor to the rather bland multiply-and-add. More on it later.

```haskell
data Neuron a = Neuron
  { weights :: Vector a
  , bias :: a
  } deriving (Eq, Show, Functor, Foldable, Traversable)

callNeuron :: (Num a) => Vector a -> (a -> a) -> Neuron a -> a
callNeuron xs activate neuron =
  activate (bias neuron + V.sum (V.zipWith (*) (weights neuron) xs))
```

<!--

I've mentioned that the computation happens in the neurons, and that each neuron accepts multiple inputs, producing one output.

All neurons compute in the same way. They multiply each input by a weight, and add the multiplied inputs together with a bias. The result of this sum is then fed into an activation function.

Why this is a good idea requires some more time, but the intuition behind this is that it combines a simple linear operation together with a non-linear function (the activation function). Without the activation function our MLP as a whole would not be able to learn interesting functions.

In Haskell this is all rather straightforward. *explain Haskell code*. Note how we're keeping all data structures and functions as general as possible -- this will come in handy later.

-->
***

## Layers

We assemble a bunch of neurons in a layer.

Each neuron takes all of the inputs of the previous layers, and outputs
a single output.

```haskell
newtype Layer a = Layer (Vector (Neuron a))
  deriving (Eq, Show, Functor, Foldable, Traversable)

callLayer :: (Num a) => Vector a -> (a -> a) -> Layer a -> Vector a
callLayer inputs activation (Layer neurons) = V.map (callNeuron inputs activation) neurons
```

<!--

We then assemble many neurons into a layer. As explained, a layer accepts some inputs (from the previous layer) and fans them out to all neurons it contains, producing as many outputs as there are neurons.

-->

***

## Putting it together

Finally, the full MLP is a series of layers:

```haskell
newtype MLP a = MLP (Vector (Layer a))
  deriving (Eq, Show, Functor, Foldable, Traversable)
```

Throughout this talk, we're always going to work with networks with a single output.

***

## Putting it together

```haskell
newtype MLP a = MLP (Vector (Layer a))
  deriving (Eq, Show, Functor, Foldable, Traversable)
```

In this talk, we're just going to use `reLU` for our activation function:

```haskell
reLU :: (Num a, Ord a) => a -> a
reLU x = if x > 0 then x else 0
```

and we're going to apply it to every output of every layer, but not to the overall output:

```haskell
callMLP :: (Num a, Ord a) => MLP a -> Vector a -> a
callMLP (MLP layers) inputs =
  V.head $ callLayer -- exactly one output!
    (foldl' (\xs -> callLayer xs reLU) inputs (V.init layers))
    id
    (V.last layers)
```

<!--

To simplify thing, we're always going to use the same activation function, `reLU`. This function sets everything less or equal than 0 to 0, and leaves the positive side unchanged.

While this function is very simple, it does the job of introducing the non-linearity we need to learn the function we're interested in, together with some properties that make it well-behaved in the context of backpropagation. It is widely used in real-world deep learning. 

We're _not_ going to use `reLU` on the output though, for reasons that will be explained later.

-->

***

## MLP recap

<div class="center-image" style="margin-top:1rem">
![](/assets/images/neural-network-diagram.svg)
</div>

* A neuron takes in $n$ inputs and spits out a single output. Its signature
    is $\mathbb{R}^n \rightarrow \mathbb{R}$. Neurons use _weights_ and _biases_ to compute.
* We combine $m$ neurons into a layer that has signature $\mathbb{R}^n \rightarrow \mathbb{R}^m$.
* We combine a bunch of layers into a MLP, with signature
    $\mathbb{R}^n \rightarrow \mathbb{R}$, where $n$ is the
    number of inputs in the first layer.

We also derived `Functor`, `Foldable`, and `Traversable`
instances for our `MLP` type. These type classes will let us inspect all the
numbers contained in the `MLP`, which are none other than the weights
and biases of each neuron of the MLP.

<!--

So let's recap the main concepts of a MLP: *read points*.

You can think of MLP as functions which compute in a very specific way. This specific way is architected to make them amenable to training.

-->

***

## Telling moons apart

<div class="center-image">![moons scatter plot](/assets/images/moons.svg)</div>

We're going to use a MLP to detect whether a point is from the upper or lower moon.

The input will be the point coordinates. 

For the output, we'll use $-1$ for the upper moon, and $1$ for the lower moon.

<!--

Let's go back to the task. We have two semicircles that "interlock" in this way. We want a classifier which tells us whether any given point is from the upper or lower moon.

We're going to build this classifier as an MLP, which accepts two inputs (the points coordinates), and outputs one output -- -1 if the point is of the upper moon, and 1 if the point is of the lower moon.

This is a good test for a neural network because some common simple classifiers do not work. We cannot find a line or convex shapes which separates the two moons. This is why we need that activation function!

-->

***

## Telling moons apart

We use a network with two layers of 16 neurons
each to solve this problem.

We haven't explained how to train the network, but for now, we'll fill it with random weights and biases:

```haskell
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC

initNeuron :: (MWC.Variate a, Num a) => MWC.GenIO -> Int -> IO (Neuron a)
initNeuron g inputs = do
  weights <- replicateM inputs (MWC.uniformR (-1, 1) g)
  return (Neuron (V.fromList weights) 0)

initLayer :: (Num a, MWC.Variate a) => MWC.GenIO -> Int -> Int -> IO (Layer a)
initLayer g inputs outputs = Layer <$> V.replicateM outputs (initNeuron g inputs)

-- | Initializes the MLP given the number of inputs of each layer.
-- The output is always a scalar.
initMLP :: (Num a, MWC.Variate a) => MWC.GenIO -> [Int] -> IO (MLP a)
initMLP g inputs =
  MLP . V.fromList <$> for (zip inputs (tail inputs ++ [1])) (uncurry (initLayer g))

-- Takes two coordinates as input.
initMoonClassifier :: (Num a, MWC.Variate a) => MWC.GenIO -> IO (MLP a)
initMoonClassifier g = initMLP gen [2, 16, 16]
```

***

## Training

To "train a network" means to find weights and biases so that our network behaves how we want it to.

In our case, we'll want weights that correctly classify points from the two moons.

The rest of the talk explains how we achieve this.

***

## Automatic differentiation

_Reverse mode automatic differentiation_ is a key algorithm that allows us to train neural-network.

If we have a function $f : \mathbb{R}^n \rightarrow \mathbb{R}$

$$y = f(x_1, \dots, x_n)$$

AD gives us, for any specific input, the partial derivatives (or gradients):

$$\dfrac{\partial f}{\partial x_1} \cdots \dfrac{\partial f}{\partial x_n}$$

***

## Automatic differentiation (Haskell)

Given some function

```haskell
f :: [Double] -> Double
```

we expect something like

```haskell
-- | Given a function with many inputs and one output, and a specific
-- set of inputs, gives back the gradient for each input.
ad :: ([Double] -> Double) -> [Double] -> [Double]
```

***

## Automatic differentiation (`ad`)

This is essentially what the [`grad`](https://www.stackage.org/haddock/lts-17.6/ad-4.4.1/Numeric-AD-Mode-Reverse.html#v:grad)
function from `ad` gives us:

```haskell
-- The grad function calculates the gradient of a non-scalar-to-scalar
-- function with reverse-mode AD in a single pass.
grad ::
     (Traversable f, Num a)
  => (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -> f a -> f a
```

For example:

```
% stack repl --package ad
> import Numeric.AD.Mode.Reverse
> grad (\[x,y] -> x * y + sin x) [1,2]
[2.5403023058681398, 1.0]
> grad (\[x,y,z] -> x * y * z) [1, 2, 3]
[6,3,2]
```

***

## Ascending or descending gradients

Let's consider the function

$$f(x, y) = 0.5 \times \mathrm{cos}(\frac{x}{2}) + \mathrm{sin}(\frac{y}{4})$$

and focus on the point $(-3\pi, \pi)$:

<div class="center-image">
![](/assets/images/gradient-example-1.svg)
</div>

***

## Using `ad` for gradient descent

`ad` also contains a useful helper function,
[`gradWith`](http://hackage.haskell.org/package/ad-4.4.1/docs/Numeric-AD.html#v:gradWith),
which lets us update the arguments of the function with their gradients:

```haskell
gradWith ::
     (Traversable f, Num a)
  => (a -> a -> b) -- ^ Function to update the arguments
  -> (forall s. Reifies s Tape => f (Reverse s a) -> Reverse s a) -- ^ Function we're optimizing
  -> f a -- ^ Inputs to the function
  -> f b
```

***

## Using `ad` for gradient descent

We can use `gradWith` to iteratively make progress towards
a peak:

    > let f [x, y] = 0.5 * cos (x / 2) + sin (y / 4)
    > let gamma = 4
    > let points pt = pt : points (gradWith (\x dx -> x + gamma * dx) f pt)
    > mapM_ (putStrLn . show) (take 5 (points [-3*pi, pi]))
    [-9.42477796076938,3.141592653589793]
    [-10.42477796076938,3.8486994347763406]
    [-11.302360522659752,4.420436439971677]
    [-11.89312422747638,4.869473384679192]
    [-12.22342592418437,5.2155893225844725]

<div class="center-image">![](/assets/images/gradient-example-3.svg)</div>

***

## Backpropagation

Backpropagation is **using AD-powered gradient-descent** to tune the weights and biases of a neural network.

However, what shall we do gradient-descent on?

***

## Loss functions

We want to do gradient-descent to have a neural network get better at some task.

In our case, we want to have our classifier of signature $\mathbb{R}^2 \rightarrow \mathbb{R}$ to detect our points correctly.

Solution: define a function measuring the difference between the results that we get and the results of some _training data_, and perform gradient descent to minimize that difference.

***

## Loss functions

So we'll run gradient descent with the **weights and biases as the inputs**, and the **difference to the training data as the output**.

In the case of our network, we have

* A first layer made out of $16$ neurons with $2$ inputs, for a total of $16 \times 2$ weights and $16$ biases;
* A second layer made out of $16$ neurons with $16$ inputs (the outputs to the previous layer), for a total of $16 \times 16$ weights and $16$ biases;
* A third layer made out of $1$ neuron with $16$ inputs, for a total of $16$ weights and $1$ bias.

For a loss function of signature

$$\mathrm{loss} : \mathbb{R}^{337} \rightarrow \mathbb{R}$$

***

## **Our** loss function

We define our loss to work on any `MLP` plus some expected input-output pairs:

```haskell
loss :: (Fractional a, Ord a) => MLP a -> Vector (Vector a, a) -> a
loss mlp samples = let
  mlpOutputs = V.map (callMLP mlp . fst) samples
  -- svm "max margin" loss
  losses = V.zipWith
    (\(_, expectedOutput) mlpOutput -> reLU (1 + (- expectedOutput) * mlpOutput))
    samples
    mlpOutputs
  dataLoss = V.sum losses / fromIntegral (V.length losses)
  -- L2 regularization
  alpha = 1e-4
  regLoss = alpha * sum (fmap (\p -> p * p) mlp)
  in dataLoss + regLoss
```

**This** is the function we'll do gradient descent on, not the function that we want the MLP to learn.

<!--

* We first compute the output that the MLP gives us for each of the
    input in the training data.
* We compute the average [_hinge loss_](https://en.wikipedia.org/wiki/Hinge_loss) for
    our samples. This loss measure is useful for classifiers that should return $-1$ or $1$.
    Intuitively, the loss will linearly increase as the MLP output disagrees
    with the expected output.
* We perform [_L2 regularization_](https://en.wikipedia.org/wiki/Regularization_(mathematics))
    on the parameters of the MLP, using its `Functor` instance. Intuitively, this helps
    to keep the weights and biases small, which results in "simpler" and hopefully
    more general models.
* We add these two terms to get the overall loss.

-->
***

## Backpropagation -- one step

Given our loss function, we can define how one step of gradient descent looks, given some training data:

```haskell
import qualified Numeric.AD.Mode.Reverse as AD

optimizeStep ::
     (Floating a, Ord a)
  => MLP a -> Vector (Vector a, a) -> a -> MLP a
optimizeStep mlp samples learningRate =
  AD.gradWith
    (\x dx -> x - learningRate * dx)
    (\ad_mlp -> loss
        ad_mlp
        (V.map (\(inputs, output) -> (V.map AD.auto inputs, AD.auto output)) samples))
    mlp
```

Note how the automatically derived `Traversable` lets us use `gradWith` directly on the `MLP`.

***

## Backpropagation -- stochastic

And how to train the network with many pieces of training data (usually called _batches_):

```haskell
-- | Given an initial model, and batches to train the model on,
-- iteratively trains the model returning each intermediate one.
optimize ::
     (Floating a, Ord a)
  => MLP a -> [Vector (Vector a, a)] -> [MLP a]
optimize mlp batches =
  scanl
    (\mlp (epoch, batch) -> let
        learningRate = 1.0 - 0.9 * fromIntegral epoch / fromIntegral numEpochs
        in optimize mlp batch learningRate)
    mlp0
    (zip [0 :: Int ..] batches)
  where
    numEpochs = length batches
```

***

## Demo

<div class="center-image">![backpropagation animation](/assets/images/mlp-animation.gif)</div>

***

## Questions?

<script>
  console.debug("Enabling presentation mode");
  window.enablePresentationMode();
</script>
