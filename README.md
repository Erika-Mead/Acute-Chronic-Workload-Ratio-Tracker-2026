# Acute Chronic Workload Ratio Tracker 2026
This app calculates the acute chronic workload ratio based on the intensity and duration of activities that the user inputs (multiple activities can be tracked). The output features a graph, analysis, and a table. The app both analyzes past activities and helps the user manage future activities. I created this app to track my activities while I was training for tennis and cross-country.

## What Does It Do?
- Records and saves the intensity and duration of multiple activities to a google sheet.
- Calculates the acute and chronic workloads and the acute chronic workload ratio.
- Displays the workload with a line graph and bar graph, showing if you are in an overtraining, undertraining, or optimal training zone.
- Analyzes and shows the acute and chronic workloads to determine if the acute chronic workload ratio is in an overtraining, undertraining, or optimal zone, recommends an optimal intensity and duration based on the intensity or duration of the next activity.
- Organizes activities in a table to make them easier to edit and delete.
<img width="2732" height="1721" alt="image" src="https://github.com/user-attachments/assets/90903a09-fb58-4b06-a987-8cd4a03a1df9" />

## How Does It Work?
- The user needs to create a google sheet with tabs for data, activities, and colors. The full url will be used for the sheet id.
- The user needs to create a cloud google service account and download the json key on google cloud console.
- The user needs to share the google sheet with the google service account.
- The user needs to put the json file in their R project folder.
- The user needs to test authentication in R studio. 
<img width="1558" height="1507" alt="image" src="https://github.com/user-attachments/assets/ce7c1b52-7b3f-4bb6-8e65-969c16f63983" />

## Limitations
- The app was designed for only one user at a time. In the future, it can be expanded to multiple users. 
