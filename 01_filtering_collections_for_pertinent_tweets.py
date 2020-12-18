# Notebook for testing / debugging the tweet-filtering python script
# AUTHORED BY MEEEEEE

# imports
import glob
from smappdragon import JsonCollection
import logging
from multiprocessing import Pool
import json

def date_handler(obj):
    return obj.isoformat() if hasattr(obj, 'isoformat') else str(obj)

def write_files(collection):
    hashtags = ["#ivoted", "#myvote2016", "#myvote"]
    statements = ["i voted", "i will vote", "my vote", "vote for"]

    with open("/scratch/olympus/projects/hashtag_filtering/hashtags_{}.json".format(collection[0].split('/')[4]), 'w') as hashtag_file:
        with open("/scratch/olympus/projects/hashtag_filtering/statements_{}.json".format(collection[0].split('/')[4]), 'w') as statement_file:
            # with open --> this is how to read in / initialize files in python
            # 'w' : write, 'r' : read
            for each_file in collection:
                hashtags_counter = 0
                statements_counter = 0
                collection = JsonCollection(each_file, throw_error=False, verbose=1)
                for tweet in collection.get_iterator():
                    if tweet and tweet["text"]:
                        if any(hashtag in tweet["text"] for hashtag in hashtags):
                            hashtag_file.write("{}\n".format(json.dumps(tweet, default=date_handler)))
                            hashtags_counter += 1
                        if any(statement in tweet["text"] for statement in statements):
                            statement_file.write("{}\n".format(json.dumps(tweet, default=date_handler)))
                            statements_counter += 1
                    else:
                        logging.info("Something was wrong with a tweet")
                logging.info("Extracted {} tweets to the statement file".format(statements_counter))
                logging.info("Extracted {} tweets to the hashtags file".format(hashtags_counter))
                # same as tweet_counter = tweet_counter + 1
        statement_file.close()
    hashtag_file.close()

def main():

    # logging
    logging.basicConfig(filename="prince_job_logfile", level=logging.INFO)
    logger = logging.getLogger(__name__)
    logging.info("This is a test to make sure that the logging is working")


    # variables
    collection_names = ["us_election_general_2016", "us_election_jebbush_2016", "us_election_others_2016", "us_election_hillary_2016", "us_election_marcorubio_2016", "us_election_tedcruz_2016", "us_election_trump_2016"]

    # creating an array with all the files to be read in
    files = []

    for each_collection in collection_names:
        collection_files = glob.glob("/scratch/nl1676/olympus_local/{}/json/*.json".format(each_collection))
        files.append(collection_files) # appending collection_files to 'files' object

    # len(files) shows the length of the top-level array
    # len(files[0]) shows the length of the first second-level array within the array, starting to count at 0!!!

    p = Pool(3)
    p.map(write_files, files)

main()
