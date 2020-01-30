import pandas as pd

def load_data(dataFolderName = '../data_luc/video/'):
	listVideoName =   ['x264-1908-bridgefar-wasm/x264-results1.csv', 
		           'x264-1908-ice-wasm/x264-results1.csv',
		           'x264-1908-flower-wasm/x264-results1.csv', 
		           'x264-1908-caire-wasm/x264-results1.csv',
		           'x264-0208-sintel-calda-wasm/x264-results1.csv', 
		           'x264-1908-footballcif-wasm/x264-results1.csv',
		           'x264-0308-crowd_run-wasm/x264-results1.csv', 
		           'x264-0608-blue-wasm/x264-results1.csv',
		           'x264-0608-people-wasm/x264-results1.csv', 
		           'x264-1908-sunflowers-wasm/x264-results1.csv',
		           'x264-0408-deadline-wasm/x264-results1.csv', 
		           'x264-2108-bridgeclose-wasm/x264-results1.csv',
		           'x264-1908-husky-wasm/x264-results1.csv', 
		           'x264-1908-tennis-wasm/x264-results1.csv',
		           'x264-1908-riverbed-wasm/x264-results1.csv', 
		           'x264-0608-park-wasm/x264-results1.csv',
		           'x264-0508-soccer-wasm/x264-results1.csv']

	# creation of the list of videos (for each video: x264 configurations + measurements)
	listVideo = []

	for vn in listVideoName:
	    listVideo.append(pd.read_csv(open(dataFolderName+vn,"r")))

	print(listVideo[0].head())

	# test
	print("There are " + str(len(listVideo)) + " videos")
	assert(len(listVideoName) == len(listVideo))

	return listVideo

