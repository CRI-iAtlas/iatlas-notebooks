The CRI-iAtlas Notebooks
========================

**The Cancer Research Institute (CRI) iAtlas** is an interactive web platform and set of analytic tools for studying interactions between tumors and the immune microenvironment. These tools allow researchers to explore associations between a variety of characterizations of immune response and genomic and clinical phenotypes.

In this repository, you'll find a collection of notebooks that you can run to carry out functionality made available by the iAtlas platform, as well as run and modify to carry out additional functions, such as  enabling the integration of *your* data with what's already in iAtlas. Additionally, you're able to easily visualize the results using our library of plotting functions. You will also find a set of notebooks that illustrate machine learning for possible determinants of molecular response to immune checkpoint inhibition (ICI) therapy, that carry out iAtlas functions using iAtlas's API and database.

Please see `our landing page`_ for more information. If you'd like to contribute please open an issue or create a pull request.


Notebooks can be run locally (with local package installation), or on the cloud, with MyBinder (with pre-compiled libraries).

.. image:: https://mybinder.org/badge_logo.svg
 :target: https://mybinder.org/v2/gh/CRI-iAtlas/iatlas-notebooks/HEAD

If running in a browser, Firefox is recommended (due to a particular conflict with plotly R library and Chrome).

.. raw:: html

    <p align="left">
        <a href="https://cri-iatlas.org">
            <img src="https://cri-iatlas.org/wp-content/uploads/2022/02/lt-iatlas-logo.png"
             width="300px" alt="iatlas logo">
        </a>
    </p>



**Notebooks**

    1. `Cell content barplots`_
        plot estimated cell type contents against samples in our database

    2. `Clinical outcomes`_
        plot survival curves comparing your data to samples in our database
    
    3. `Immune feature trends`_
        investigate using our collection of immune features

    4. `ICI Models - Elastic Net`_
        test prediction of molecular response to immune checkpoint inhibition (ICI) with elastic net regression

    5. `ICI Models - Logistic Regression`_
        test prediction of molecular response to ICI with logistic regression models
    
    6. `ICI Models - Random Forest`_
        test prediction of molecular response to ICI with random forest regression models

    7. `ICI Models - XGboost`_
        test prediction of molecular response to ICI with gradient boosting



**How to Cite Us**

If you find these notebooks useful, please cite the CRI iAtlas paper (PubMed: `33214875`_). Thank you!

    Eddy JA, Thorsson V, Lamb AE, Gibbs DL, Heimann C, Yu JX, Chung V, Chae Y, Dang K, Vincent BG, Shmulevich I, Guinney J. 
    CRI iAtlas: an interactive portal for immuno-oncology research. F1000Research 2020, 9:1028.


and follow @iatlas_cri on Twitter for latest developments!


.. _our landing page: https://cri-iatlas.org

.. _Cell content barplots: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/cell_content_barplots.ipynb

.. _Clinical outcomes: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/clinical_outcomes.ipynb

.. _Immune feature trends: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/immune_feature_trends.ipynb

.. _ICI Models - Elastic Net: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_elastic_net.ipynb

.. _ICI Models - Logistic Regression: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_logistic_regression.ipynb

.. _ICI Models - Random Forest: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_random_forest.ipynb

.. _ICI Models - XGboost: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_xgboost.ipynb

.. _33214875: https://pubmed.ncbi.nlm.nih.gov/33214875/
