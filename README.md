# TripCraft ✈️🗺️

**TripCraft** is an intelligent travel itinerary generation package developed as part of the **MTH209: Data Science Lab II** course project at IIT Kanpur.
The package creates **personalised day-wise travel plans across Indian cities** by considering user preferences, budget constraints, and attraction proximity, enabling efficient and customised trip planning.

## Features

* 📍 **Personalised Attraction Recommendations** based on user interests and preferences.
* 🗓️ **Automatic Day-wise Itinerary Generation** for efficient trip scheduling.
* 🧭 **Optimised Route Planning** by grouping nearby attractions together.
* 💰 **Budget Estimation and Cost Breakdown** for better financial planning.
* 🗺️ **Interactive Maps and Visualisations** through an intuitive Shiny interface.
* 📊 **Attraction Landscape Visualisation** using dimensionality reduction techniques.

## Methodology

### 1. Attraction Clustering

TripCraft uses **K-Means Clustering** to group geographically close attractions into clusters, helping generate efficient daily travel schedules while minimising travel time.

### 2. Personalised Recommendations

A **k-Nearest Neighbours (kNN)** based recommendation engine suggests attractions that align with user preferences and interests.

### 3. Data Visualisation

**Principal Component Analysis (PCA)** is employed to visualise attraction landscapes and identify underlying patterns in travel destinations.

### 4. Interactive User Interface

The project integrates an **R Shiny** application that provides:

* Interactive maps
* Travel distance calculations
* Budget summaries
* Day-wise itinerary views

## Tech Stack

* **R**
* **R Shiny**
* **K-Means Clustering**
* **k-Nearest Neighbours (kNN)**
* **Principal Component Analysis (PCA)**

## Results

TripCraft delivers an end-to-end intelligent trip planning solution that:

* Automates itinerary generation.
* Provides personalised travel recommendations.
* Optimises routes for better travel efficiency.
* Offers detailed cost insights for informed decision-making.
