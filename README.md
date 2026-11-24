# Acute-Chronic-Workload-Ratio-Tracker-2026
This app calculates the acute chronic workload ratio based on the intensity and duration of activities that the user inputs (multiple activities can be tracked). The output features a graph, analysis, and a table. The app both analyzes past activities and helps the user manage future activities. I created this app to track my activities while I was training for tennis and cross-country so the app can currently only be used by one person at a time.

## What-Does-It-Do?
- records and saves the intensity and duration of multiple activities to a google sheet
- calculates the acute and chronic workloads and the acute chronic workload ratio
- displays the workload with a line graph and bar graph, showing if you are in an overtraining, undertraining, or optimal training zone
- analyzes and shows the acute and chronic workloads to determine if the acute chronic workload ratio is in an overtraining, undertraining, or optimal zone, recommends an optimal intensity and duration based on the intensity or duration of the next activity
- organizes activities in a table to make them easier to edit and delete

## How-Does-It-Work?
- the user needs to install the packages so the packages can run
- when the user opens the app, the app pulls the data from the google sheet, which is enabled by the json authentication of the google service account that connects to the google sheet
- when the user records, edits, saves, or deletes the intensity and duration of activities, the app changes the data in the google sheet

## Limitations
- since the app requires json authentication, it can only be used by one user - in the future, it can be expanded to multiple users
