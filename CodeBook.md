# Code book for Coursera *Getting and Cleaning Data* course project

The data set for this codebook is the `tidy_data.txt`, a resulting text file from `run_analysis.R`.

See the `README.md` file to learn more about `run_analysis.R`.

## Variables

Each row contains, for a given subject and activity, 79 averaged signal measurements.

### Identifiers

- `subject`

Variable name    | Description
-----------------|------------
subject          | ID of the 30 volunteers who performed the activity. Its range is from 1 to 30, integer.

- `activity`

	Activity identifier, string with 6 possible values: 

Variable name    | Description
-----------------|------------
activity         |  Activity type that the 30 volunteers  who performed the activity. 
                 |  Its has 6 levels:
                 |   1. WALKING - subject is walking
                 |   2. WALKING_UPSTAIRS - subject is walking upstairs
                 |   3. WALKING_DOWNSTAIRS - subject is walking downstairs
                 |   4. SITTING - subject is sitting
                 |   5. STANDING - subject is standing
                 |   6. LAYING - subject is laying

### Measurements

#### Time-domain signals

- Average time-domain body acceleration in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerMeanX`
	- `timeDomainBodyAccelerometerMeanY`
	- `timeDomainBodyAccelerometerMeanZ`

- Standard deviation of the time-domain body acceleration in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerStandardDeviationX`
	- `timeDomainBodyAccelerometerStandardDeviationY`
	- `timeDomainBodyAccelerometerStandardDeviationZ`

- Average time-domain gravity acceleration in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainGravityAccelerometerMeanX`
	- `timeDomainGravityAccelerometerMeanY`
	- `timeDomainGravityAccelerometerMeanZ`

- Standard deviation of the time-domain gravity acceleration in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainGravityAccelerometerStandardDeviationX`
	- `timeDomainGravityAccelerometerStandardDeviationY`
	- `timeDomainGravityAccelerometerStandardDeviationZ`

- Average time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerJerkMeanX`
	- `timeDomainBodyAccelerometerJerkMeanY`
	- `timeDomainBodyAccelerometerJerkMeanZ`

- Standard deviation of the time-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions, in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerJerkStandardDeviationX`
	- `timeDomainBodyAccelerometerJerkStandardDeviationY`
	- `timeDomainBodyAccelerometerJerkStandardDeviationZ`

- Average time-domain body angular velocity in the X, Y and Z directions, unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeMeanX`
	- `timeDomainBodyGyroscopeMeanY`
	- `timeDomainBodyGyroscopeMeanZ`

- Standard deviation of the time-domain body angular velocity in the X, Y and Z directions, unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeStandardDeviationX`
	- `timeDomainBodyGyroscopeStandardDeviationY`
	- `timeDomainBodyGyroscopeStandardDeviationZ`

- Average time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions, unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeJerkMeanX`
	- `timeDomainBodyGyroscopeJerkMeanY`
	- `timeDomainBodyGyroscopeJerkMeanZ`

- Standard deviation of the time-domain body angular velocity jerk (derivation of the angular velocity in time) in the X, Y and Z directions, unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeJerkStandardDeviationX`
	- `timeDomainBodyGyroscopeJerkStandardDeviationY`
	- `timeDomainBodyGyroscopeJerkStandardDeviationZ`

- Average and standard deviation of the time-domain magnitude of body acceleration, in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerMagnitudeMean`
	- `timeDomainBodyAccelerometerMagnitudeStandardDeviation`

- Average and standard deviation of the time-domain magnitude of gravity acceleration, in *g*'s (9.81 m.s⁻²):

	- `timeDomainGravityAccelerometerMagnitudeMean`
	- `timeDomainGravityAccelerometerMagnitudeStandardDeviation`

- Average and standard deviation of the time-domain magnitude of body acceleration jerk (derivation of the acceleration in time), in *g*'s (9.81 m.s⁻²):

	- `timeDomainBodyAccelerometerJerkMagnitudeMean`
	- `timeDomainBodyAccelerometerJerkMagnitudeStandardDeviation`

- Average and standard deviation of the time-domain magnitude of body angular velocity, unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeMagnitudeMean`
	- `timeDomainBodyGyroscopeMagnitudeStandardDeviation`

- Average and standard deviation of the time-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time), unit measure radians per second (rad.s⁻¹):

	- `timeDomainBodyGyroscopeJerkMagnitudeMean`
	- `timeDomainBodyGyroscopeJerkMagnitudeStandardDeviation`

#### Frequency-domain signals

- Average frequency-domain body acceleration in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerMeanX`
	- `frequencyDomainBodyAccelerometerMeanY`
	- `frequencyDomainBodyAccelerometerMeanZ`

- Standard deviation of the frequency-domain body acceleration in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerStandardDeviationX`
	- `frequencyDomainBodyAccelerometerStandardDeviationY`
	- `frequencyDomainBodyAccelerometerStandardDeviationZ`

- Weighted average of the frequency components of the frequency-domain body acceleration in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerMeanFrequencyX`
	- `frequencyDomainBodyAccelerometerMeanFrequencyY`
	- `frequencyDomainBodyAccelerometerMeanFrequencyZ`

- Average frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerJerkMeanX`
	- `frequencyDomainBodyAccelerometerJerkMeanY`
	- `frequencyDomainBodyAccelerometerJerkMeanZ`

- Standard deviation of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerJerkStandardDeviationX`
	- `frequencyDomainBodyAccelerometerJerkStandardDeviationY`
	- `frequencyDomainBodyAccelerometerJerkStandardDeviationZ`

- Weighted average of the frequency components of the frequency-domain body acceleration jerk (derivation of the acceleration in time) in the X, Y and Z directions:

	- `frequencyDomainBodyAccelerometerJerkMeanFrequencyX`
	- `frequencyDomainBodyAccelerometerJerkMeanFrequencyY`
	- `frequencyDomainBodyAccelerometerJerkMeanFrequencyZ`

- Average frequency-domain body angular velocity in the X, Y and Z directions:

	- `frequencyDomainBodyGyroscopeMeanX`
	- `frequencyDomainBodyGyroscopeMeanY`
	- `frequencyDomainBodyGyroscopeMeanZ`

- Standard deviation of the frequency-domain body angular velocity in the X, Y and Z directions:

	- `frequencyDomainBodyGyroscopeStandardDeviationX`
	- `frequencyDomainBodyGyroscopeStandardDeviationY`
	- `frequencyDomainBodyGyroscopeStandardDeviationZ`

- Weighted average of the frequency components of the frequency-domain body angular velocity in the X, Y and Z directions:

	- `frequencyDomainBodyGyroscopeMeanFrequencyX`
	- `frequencyDomainBodyGyroscopeMeanFrequencyY`
	- `frequencyDomainBodyGyroscopeMeanFrequencyZ`

- Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration:

	- `frequencyDomainBodyAccelerometerMagnitudeMean`
	- `frequencyDomainBodyAccelerometerMagnitudeStandardDeviation`
	- `frequencyDomainBodyAccelerometerMagnitudeMeanFrequency`

- Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body acceleration jerk (derivation of the acceleration in time):

	- `frequencyDomainBodyAccelerometerJerkMagnitudeMean`
	- `frequencyDomainBodyAccelerometerJerkMagnitudeStandardDeviation`
	- `frequencyDomainBodyAccelerometerJerkMagnitudeMeanFrequency`

- Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity:

	- `frequencyDomainBodyGyroscopeMagnitudeMean`
	- `frequencyDomainBodyGyroscopeMagnitudeStandardDeviation`
	- `frequencyDomainBodyGyroscopeMagnitudeMeanFrequency`

- Average, standard deviation, and weighted average of the frequency components of the frequency-domain magnitude of body angular velocity jerk (derivation of the angular velocity in time):

	- `frequencyDomainBodyGyroscopeJerkMagnitudeMean`
	- `frequencyDomainBodyGyroscopeJerkMagnitudeStandardDeviation`
	- `frequencyDomainBodyGyroscopeJerkMagnitudeMeanFrequency`
