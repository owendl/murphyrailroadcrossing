from shiny import App, render, ui
from shiny.types import ImgData


# %%
import requests
import cv2
import json
from pathlib import Path

dir = Path(__file__).resolve().parent


# %%
with open("murphy_camera_creds.json", 'r') as file:
    data = json.load(file)

# %%
client_id = data["client_id"]
client_secret = data["client_secret"]

refresh_token = data["refresh_token"]

project_id = data["project_id"]
device_id = data["device_id"]

# %%
url = 'https://www.googleapis.com/oauth2/v4/token'
params = {
    'client_id': client_id,
    'client_secret': client_secret,
    'refresh_token': refresh_token,
    'grant_type': 'refresh_token'
}


# %%
# RTSP stream URL and access token obtained from GenerateRtspStream command

# Function to save a single frame from the RTSP stream
def save_frame(rtsp_url, token, output_path):
    # Create a VideoCapture object
    cap = cv2.VideoCapture(rtsp_url)

    # Capture a single frame
    ret, frame = cap.read()
    # Check if the frame is captured successfully
    if ret:
        # Save the frame to the specified output path
        cv2.imwrite(output_path, frame)
        print(f"Frame saved as {output_path}")
    else:
        print("Error: Failed to capture frame")

    # Release the VideoCapture object
    cap.release()


app_ui = ui.page_fluid(
    ui.layout_sidebar(
        ui.h3("Murphy & Whitehall Railroad Crossing"),
        ui.p('''This site shows a snapshot from a camera pointing at the railroad crossing at Murphy and Whitehall. Our hope is that you can use this site to inform your commuting plans, especially getting to the West End Marta Station.
             
             We would like to thank CreateATL for the donation of this camera as well as the wifi and electricity to make all of this possible.
             '''),

        ),
    ui.output_image("image")

)


def server(input, output, session):

    response = requests.post(url, params=params)
    access_token = response.json()["access_token"]

    # %%
    # Replace these variables with your actual values

    bearer_token = access_token

    # Endpoint for requesting an image capture
    image_capture_url = f"https://smartdevicemanagement.googleapis.com/v1/enterprises/{project_id}/devices/{device_id}:executeCommand"

    # Define the command to capture an image
    command_data = {
        "command": "sdm.devices.commands.CameraLiveStream.GenerateRtspStream",
        "params": {}
    }

    headers = {
        "Authorization": f"Bearer {bearer_token}",
        "Content-Type": "application/json"
    }
    response = requests.post(image_capture_url, json=command_data, headers=headers)

    results = response.json()
    stream_url = results["results"]["streamUrls"]["rtspUrl"]
    stream_token = results["results"]["streamToken"]

    # Call the function to save a single frame
    save_frame(stream_url, stream_token, str(dir/"captured_frame.jpg"))

    @output
    @render.image
    def image():
        from pathlib import Path

        dir = Path(__file__).resolve().parent

        img: ImgData = {"src": str(dir/"captured_frame.jpg")}
        return img


app = App(app_ui, server)
