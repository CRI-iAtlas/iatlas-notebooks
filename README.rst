The CRI-iAtlas Notebooks
========================

**The Cancer Research Institute (CRI) iAtlas** is an interactive web platform and set of analytic tools for studying interactions between tumors and the immune microenvironment. These tools allow researchers to explore associations between a variety of characterizations of immune response and genomic and clinical phenotypes.

In this repository, you'll find a collection of notebooks that you can run to carry out functionality made available by the iAtlas platform, as well as how carry out additional functions, such as  enabling the integration of *your* data with what's already in iAtlas. Additionally, you're able to easily visualize the results using the iAtlas library of plotting functions. You will also find a set of notebooks that illustrate machine learning for possible determinants of molecular response to immune checkpoint inhibition (ICI) therapy, which carry out iAtlas functions using iAtlas's API and database.

Please see `our landing page`_ for more information. If you'd like to contribute please open an issue or create a pull request.


Notebooks can be run locally (with local package installation), or on the cloud, with MyBinder (with pre-compiled libraries).

.. image:: https://mybinder.org/badge_logo.svg
 :target: https://mybinder.org/v2/gh/CRI-iAtlas/iatlas-notebooks/HEAD

If running in a browser, Firefox is recommended (due to a conflict with the plotly R library and Chrome).

.. raw:: html

    <p align="left">
        <a href="https://cri-iatlas.org">
            <img src="https://cri-iatlas.org/wp-content/uploads/2022/02/lt-iatlas-logo.png"
             width="300px" alt="iatlas logo">
        </a>
    </p>



**Notebooks**

    #. `Query the CRI iAtlas database`_
        Explore the data hosted by CRI iAtlas by using the API to query our database

    #. `Query Immune Checkpoint Inhibition data`_
        Explore the Immune Checkpoint Inhibition (ICI) data available in iAtlas

    #. `Query Immune Checkpoint Inhibition data - Python`_
        Explore the Immune Checkpoint Inhibition (ICI) data available in iAtlas, using Python

    #. `Cell content barplots`_
        Plot estimated cell type contents against samples in our database

    #. `Clinical outcomes`_
        Plot survival curves comparing your data to samples in our database
    
    #. `Immune feature trends`_
        Investigate using our collection of immune features
    
    #. `ICI Hazard Ratio`_
        Analyze outcome data in ICI datasets using Cox proportional hazard models

    #. `ICI Neoantigen`_
        Explore the neoantigen data available in iAtlas

    #. `ICI Models - Elastic Net`_
        Build an elastic net regression model with ICI data

    #. `ICI Models - Logistic Regression`_
        Build a logistic regression model with ICI data
    
    #. `ICI Models - Random Forest`_
        Build a Random Forest model with ICI data

    #. `ICI Models - XGboost`_
        Build a eXtreme Gradient Boosting (XGBoost) model with ICI data




**How to Cite Us**

If you find these notebooks useful, please cite the CRI iAtlas paper (PubMed: `33214875`_). Thank you!

    Eddy JA, Thorsson V, Lamb AE, Gibbs DL, Heimann C, Yu JX, Chung V, Chae Y, Dang K, Vincent BG, Shmulevich I, Guinney J. 
    CRI iAtlas: an interactive portal for immuno-oncology research. F1000Research 2020, 9:1028.


and follow @iatlas_cri on Twitter for latest developments!


.. _our landing page: https://cri-iatlas.org

.. _Query the CRI iAtlas database: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/querying_TCGA_features_and_expression.ipynb

.. _Query Immune Checkpoint Inhibition data: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_query_iatlas_data.ipynb

.. _Query Immune Checkpoint Inhibition data - Python: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_query_iatlas_data_python.ipynb

.. _Cell content barplots: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/cell_content_barplots.ipynb

.. _Clinical outcomes: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/clinical_outcomes.ipynb

.. _Immune feature trends: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/immune_feature_trends.ipynb

.. _ICI Hazard Ratio: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_hazard_ratio.ipynb

.. _ICI Neoantigen: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_neoantigen.ipynb

.. _ICI Models - Elastic Net: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_notebooks/ici_models_elastic_net.ipynb

.. _ICI Models - Logistic Regression: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_notebooks/ici_models_logistic_regression.ipynb

.. _ICI Models - Random Forest: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_notebooks/ici_models_random_forest.ipynb

.. _ICI Models - XGboost: https://github.com/CRI-iAtlas/iatlas-notebooks/blob/main/ici_models_notebooks/ici_models_xgboost.ipynb

.. _33214875: https://pubmed.ncbi.nlm.nih.gov/33214875/
